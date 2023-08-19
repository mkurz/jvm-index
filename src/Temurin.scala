object Temurin {

  def fullIndex(ghToken: String): Index = {
    val temurinIndices = Seq(21)
      .map(ver => ver -> index(ghToken, ver, adopt = false))

    val adoptiumIndices = (temurinIndices)
      .toVector
      .sortBy(_._1)
      .map(_._2)
      .map { index0 =>
        index0.mapJdkName { name =>
          val suffix = name
            .stripPrefix("jdk@adopt")
            .stripPrefix("jdk@temurin")
          "jdk@adoptium" + suffix
        }
      }

    val allIndices = 
      adoptiumIndices.iterator
    allIndices.foldLeft(Index.empty)(_ + _)
  }

  def index(
    ghToken: String,
    baseVersion: Int,
    adopt: Boolean
  ): Index = {
    val ghOrg         = if (adopt) "AdoptOpenJDK" else "adoptium"
    val projectPrefix = if (adopt) "openjdk" else "temurin"
    val ghProj        = s"$projectPrefix$baseVersion-binaries"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(_.prerelease)

    def jdkName(suffix: String = ""): String =
      "jdk@" + (if (adopt) "adopt" else "temurin") + suffix

    def assetNamePrefix(jdkStr: String) = Seq(
      s"OpenJDK${baseVersion}U-${jdkStr}_",
      s"OpenJDK$baseVersion-${jdkStr}_"
    )

    def archOpt(input: String): Option[(String, String)] =
      Map(
        "amd64"   -> "x64_",
        "x86"     -> "x86-32_",
        "arm64"   -> "aarch64_",
        "arm"     -> "arm_",
        "s390x"   -> "s390x_",
        "ppc64"   -> "ppc64_",
        "ppc64le" -> "ppc64le_"
      ).collectFirst {
        case (k, v) if input.startsWith(v) =>
          k -> input.stripPrefix(v)
      }

    def osOpt(input: String): Option[(String, String)] =
      if (input.startsWith("linux_"))
        Some(("linux", input.stripPrefix("linux_")))
      else if (input.startsWith("alpine-linux_"))
        Some(("alpine-linux", input.stripPrefix("alpine-linux_")))
      else if (input.startsWith("mac_"))
        Some(("darwin", input.stripPrefix("mac_")))
      else if (input.startsWith("windows_"))
        Some(("windows", input.stripPrefix("windows_")))
      else if (input.startsWith("aix_"))
        Some(("aix", input.stripPrefix("aix_")))
      else
        None

    def archiveTypeOpt(input: String): Option[String] =
      if (input == "zip") Some("zip")
      else if (input == "tar.gz") Some("tgz")
      else None

    val prefixes =
      if (baseVersion == 8) Seq("jdk8u")
      else Seq(s"jdk-$baseVersion.", s"jdk-$baseVersion+") //, "jdk20u-2023-08-18-14-05-beta")
    val indices = releases0
      .filter { release =>
        prefixes.exists(prefix => release.tagName.startsWith(prefix))
      }
      .flatMap { release =>
        val version0 = release.tagName.stripPrefix("jdk-").stripPrefix("jdk")
        //println("---:\n" + version0)
        val versionInFileName =
          if (version0.contains("+"))
            version0.split('+') match {
              case Array(before, after) => s"${before}_${after.takeWhile(_ != '.')}"
              case _                    => version0
            }
          else version0
        //println(versionInFileName + "\n")
        val assets = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.tagName).to(LazyList)
        def index(jdkName: String, assetNamePrefix: Seq[String]) = assets
          .iterator
          .filter(asset => assetNamePrefix.exists(asset.name.startsWith))
          .flatMap { asset =>
            val name0 = assetNamePrefix.foldLeft(asset.name)(_ stripPrefix _)
            //println(name0)
            val opt = for {
              (arch, rem) <- archOpt(name0)
              (os, rem0)  <- osOpt(rem)
              ext <- {
                val prefix = "hotspot_ea_" + versionInFileName.replace("_", "-0-").stripSuffix("-ea-beta") + "."
                //println("~~")
                //println(prefix)
                //println(rem0)
                //println("~~~~")
                Some(rem0)
                  .filter(_.startsWith(prefix))
                  .map(_.stripPrefix(prefix))
              }
              archiveType <- archiveTypeOpt(ext)
            } yield Index(os, arch, jdkName, "1." + version0.stripSuffix("-ea-beta").replace("+", "-ea"), archiveType + "+" + asset.downloadUrl)
            opt.toSeq
          }
        def releaseIndex = index(jdkName(), assetNamePrefix("jdk"))
        releaseIndex
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}

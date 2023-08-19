//> using scala "2.13"
//> using lib "com.softwaremill.sttp.client3::core:3.8.15"
//> using lib "com.lihaoyi::ujson:3.1.0"
//> using lib "com.lihaoyi::os-lib:0.9.1"
//> using option "-deprecation"
//> using option "-Ywarn-unused"

object GenerateIndex {

  def main(args: Array[String]): Unit = {

    val output = "index.json"

    val adoptIndex0         = Temurin.fullIndex(GhToken.token)

    val json =
      (adoptIndex0).json
    val dest = os.Path(output, os.pwd)
    os.write.over(dest, json)
    System.err.println(s"Wrote $dest")
  }
}

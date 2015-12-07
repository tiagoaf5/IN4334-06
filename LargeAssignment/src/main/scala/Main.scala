import ch.usi.inf.reveal.parsing.artifact._

object Main {
  def main(args: Array[String]): Unit = {
    val artifact = ArtifactSerializer.deserializeFromFile(getClass.getResource("/10000008.json").getPath)

    print(artifact.question.id)
  }
}

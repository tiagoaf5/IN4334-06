import java.io.File

import scala.collection.mutable
import scala.io.Source

/**
  * Created by luiscleto on 14/01/2016.
  */
object GitHubPopularity {
  val resourceDir = "src/main/resources/"
  val tagFileName = "github_popularity.csv"


  var tagPopularity: scala.collection.mutable.Map[String, Long] = mutable.HashMap[String, Long]().withDefaultValue(0)
  loadTagsFromFile()

  def loadTagsFromFile(): Unit = {
    if (new File(resourceDir+tagFileName).exists())
      for (line <- Source.fromFile(resourceDir+tagFileName).getLines() if line.nonEmpty)
        tagPopularity += (line.split(";")(0) -> line.split(";")(1).toLong)
    else
      throw new Exception("GitHub Popularity file not found!")
  }

  def getTagPopularity(tagName: String): Long =
    tagPopularity.getOrElse(tagName, 0)
}

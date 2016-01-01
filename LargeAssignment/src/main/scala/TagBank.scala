import java.io.{File, FileWriter, FileOutputStream, PrintWriter}

import scala.collection.immutable.HashMap
import scala.io.Source
import scalaj.http.Http

/**
  * Created by luiscleto on 01/01/2016.
  */
object TagBank {
  val resourceDir = "src/main/resources/tag_bank/"
  val tagFileName = "tag_popularity.msr"


  var tagPopularity: Map[String, Long] = new HashMap[String, Long]
  loadTagsFromFile()

  def loadTagsFromFile(): Unit = {
    if (new File(resourceDir+tagFileName).exists())
      for (line <- Source.fromFile(resourceDir+tagFileName).getLines() if line.nonEmpty)
        tagPopularity += (line.split("\\s+")(0) -> line.split("\\s+")(1).toLong)
  }

  def saveTagsToFile(): Unit = {
    new File(resourceDir).mkdirs()
    val fWriter: FileWriter = new FileWriter (resourceDir+tagFileName, false)
    for ((k, v) <- tagPopularity)
      fWriter.write(k + " " + v.toString + "\n")
    fWriter.close()
  }

  def getTagPopularity(tagName: String): Long =
    tagPopularity.get(tagName) match {
      case Some(pop) =>  pop
      case None => addToTagCollection(tagName)
    }

  def addToTagCollection(tagName: String): Long = {
    print("Fetching new tag: " + tagName + "... ")
    var pop: Long = 0

    val response = Http("https://api.stackexchange.com/2.2/search?filter=total&order=desc&site=stackoverflow&sort=activity&tagged="+tagName).asString
    if (response.code != 200)
      throw new Exception("stack exchange API failed to reply: code " + response.code.toString)

    pop = response.body.split("[^0-9]").filter(_.length > 0).head.toLong

    println(tagName + ": " + pop.toString)

    tagPopularity += (tagName -> pop)
    saveTagsToFile()
    pop
  }
}

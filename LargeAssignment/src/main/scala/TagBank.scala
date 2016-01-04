import java.io.{File, FileWriter, FileOutputStream, PrintWriter}

import scala.collection.mutable
import scala.io.{StdIn, Source}
import scalaj.http.{HttpResponse, Http}

/**
  * Created by luiscleto on 01/01/2016.
  */
object TagBank {
  val resourceDir = "src/main/resources/tag_bank/"
  val tagFileName = "tag_popularity.msr"


  var tagPopularity: scala.collection.mutable.Map[String, Long] = new mutable.HashMap[String, Long]
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
    var response: HttpResponse[String] = null

    while (response == null) {
      try
        response = Http("https://api.stackexchange.com/2.2/search?filter=total&order=desc&site=stackoverflow&sort=activity&tagged=" + tagName).asString
      catch {
        case _: Exception => response = null
      }
    }
    while (response.code != 200) {
      System.err.println("Stack exchange API failed to reply: code " + response.code.toString)
      System.err.print("Try again? (y/X)")
      val reply = StdIn.readChar()
      if (reply.toLower == 'y') {
        response = null
        while (response == null) {
          try
            response = Http("https://api.stackexchange.com/2.2/search?filter=total&order=desc&site=stackoverflow&sort=activity&tagged=" + tagName).asString
          catch {
            case _: Exception => response = null
          }
        }
      }
      else
        throw new Exception("stack exchange API failed to reply: code " + response.code.toString)
    }

    pop = response.body.split("[^0-9]").filter(_.length > 0).head.toLong

    println(tagName + ": " + pop.toString)

    tagPopularity += (tagName -> pop)
    saveTagsToFile()
    pop
  }
}

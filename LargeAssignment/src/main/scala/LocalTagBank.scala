import java.io.{File, FileWriter}
import java.nio.file.{Path, Paths, Files}
import java.util.function.{Predicate, Consumer}

import ch.usi.inf.reveal.parsing.artifact.ArtifactSerializer

import scala.collection.mutable
import scala.io.{Source, StdIn}
import scalaj.http.{Http, HttpResponse}

/**
  * Created by luiscleto on 01/01/2016.
  */
class LocalTagBank(val resourceDir: String) {
  implicit def toConsumer[A](function: A => Unit): Consumer[A] = new Consumer[A]() {
    override def accept(arg: A): Unit = function.apply(arg)
  }
  implicit def toPredicate[A](function: A => Boolean): Predicate[A] = new Predicate[A]() {
    override def test(arg: A): Boolean = function.apply(arg)
  }
  val tagFileName = "tag_popularity.msr"


  var tagPopularity: scala.collection.mutable.Map[String, Long] = new mutable.HashMap[String, Long]
  loadTagsFromFile()

  def loadTagsFromFile(): Unit = {
    if (new File(resourceDir+"/"+tagFileName).exists())
      for (line <- Source.fromFile(resourceDir+"/"+tagFileName).getLines() if line.nonEmpty)
        tagPopularity += (line.split("\\s+")(0) -> line.split("\\s+")(1).toLong)
    else readTagCollection()
  }

  def saveTagsToFile(): Unit = {
    val fWriter: FileWriter = new FileWriter (resourceDir+"/"+tagFileName, false)
    for ((k, v) <- tagPopularity)
      fWriter.write(k + " " + v.toString + "\n")
    fWriter.close()
  }

  def getTagPopularity(tagName: String): Long =
    tagPopularity.get(tagName) match {
      case Some(pop) =>  pop
      case None => throw new Exception("Tag not found: " + tagName)
    }

  def readTagCollection(): Unit = {
    print("Loading tags... ")

    Files.walk(Paths.get(resourceDir))
      .filter({(f:Path) => f.toString.endsWith(".json")})
      .forEach({(f:Path) => ArtifactSerializer.deserializeFromFile(f.toString).question.tags
        .foreach(t => tagPopularity += (t -> (1 + tagPopularity.getOrElse[Long](t, 0))))
      })

    saveTagsToFile()

    println("Tags loaded!")
  }
}

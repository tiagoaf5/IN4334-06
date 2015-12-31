import java.nio.file.{Path, Paths, Files}
import java.util.Calendar
import java.util.function.{Predicate, Consumer}

import ch.usi.inf.reveal.parsing.artifact._

object Main {
  implicit def toConsumer[A](function: A => Unit): Consumer[A] = new Consumer[A]() {
    override def accept(arg: A): Unit = function.apply(arg)
  }
  implicit def toPredicate[A](function: A => Boolean): Predicate[A] = new Predicate[A]() {
    override def test(arg: A): Boolean = function.apply(arg)
  }

  def main(args: Array[String]): Unit = {
    //val stormedDataSetDir = "D:\\FEUP\\erasmus\\MSREP\\stormed-dataset\\sample"
    val stormedDataSetDir = "src/main/resources/input"

    val tagFilters = List("java", "android")
    val analyser = new DiscussionAnalyser("src/main/resources/results/" + Calendar.getInstance().getTimeInMillis + "_filtered-by_" + tagFilters.mkString("_") + ".csv", tagFilters)

    Files.walk(Paths.get(stormedDataSetDir))
      .filter({(f:Path) => f.toString.endsWith(".json")})
      .forEach({(f:Path) => analyser.processDiscussion(ArtifactSerializer.deserializeFromFile(f.toString))})

    analyser.finish()
  }

}





import java.nio.file.{Path, Paths, Files}
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
    val stormedDatasetDir = getClass.getResource(".").getPath.substring(1) //define for folder with stormed json files

    var score = 0.0
    var numfiles = 0
    Files.walk(Paths.get(stormedDatasetDir)).filter({(f:Path) => f.toString.endsWith(".json")}).forEach({(f:Path) =>
      val artifact = ArtifactSerializer.deserializeFromFile(f.toString)
      score *= numfiles
      score += artifact.question.score
      numfiles += 1
      score = score / numfiles

      println(artifact.question.score + " - " + score)
    })
  }

}
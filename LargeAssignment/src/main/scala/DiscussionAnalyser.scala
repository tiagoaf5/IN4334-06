import java.io.{PrintWriter, File}

import ch.usi.inf.reveal.parsing.artifact.StackOverflowArtifact

/**
  * Created by luiscleto on 08/12/2015.
  */
class DiscussionAnalyser(filename: String) {
  var numFiles = 0
  var scoreSum = 0
  var tagCountSum = 0

  val pw = new PrintWriter(filename)
  pw.write("id,score,tag_count,num_answers\n")

  def finish(): Unit = {
    pw.close()
  }

  def processDiscussion(artifact: StackOverflowArtifact): Unit = {
    numFiles += 1
    scoreSum += artifact.question.score
    tagCountSum += artifact.question.tags.length

    pw.println(Array(artifact.id.toString, artifact.question.score, artifact.question.tags.length, artifact.answers.length).mkString(","))
  }
}

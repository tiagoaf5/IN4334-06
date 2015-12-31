import java.io.{PrintWriter, File}

import ch.usi.inf.reveal.parsing.artifact.StackOverflowArtifact

/**
  * Created by luiscleto on 08/12/2015.
  */
class DiscussionAnalyser(filedir: String, filename: String, tagFilters: Seq[String]) {
  var numFiles = 0

  val pw_questions = new PrintWriter(filedir + "questions_" + filename)
  pw_questions.write("title length," +  //TODO
    "tags count," + //TODO
    "tags popularity," + //TODO
    "java %," + //TODO
    "json %," + //TODO
    "xml %," + //TODO
    "stack traces %," + //TODO
    "length," + //TODO
    "words count," + //TODO
    "day of week," + //TODO
    "reputation," + //TODO
    "intercalations," + //TODO
    "score" + //TODO
    "number of answers" + //TODO
    "max answer score" + //TODO
    "avg answer score" + //TODO
    "min answer score" + //TODO
    "number of comments" + //TODO
    "max answer length" + //TODO
    "avg answer length" + //TODO
    "min answer length\n") //TODO

  val pw_answers = new PrintWriter(filedir + "answers_" + filename)
  pw_answers.write("first posted," +  //TODO
    "same day as question," + //TODO
    "java %," + //TODO
    "json %," + //TODO
    "xml %," + //TODO
    "stack traces %," + //TODO
    "length," + //TODO
    "words count," + //TODO
    "day of week," + //TODO
    "reputation," + //TODO
    "intercalations," + //TODO
    "score" + //TODO
    "number of comments" + //TODO
    "max comment length" + //TODO
    "avg comment length" + //TODO
    "min comment length\n") //TODO

  def finish(): Unit = {
    pw_questions.close()
    pw_answers.close()

    System.out.println("Analyzed a total of " + numFiles + " discussions.")
  }

  def processDiscussion(artifact: StackOverflowArtifact): Unit = {

    if(!tagFilters.forall(artifact.question.tags.contains))
      return

    numFiles += 1

    pw_questions.println(Array(artifact.id.toString, artifact.question.score, artifact.question.tags.length, artifact.answers.length).mkString(","))
  }
}

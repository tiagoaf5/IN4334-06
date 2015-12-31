import java.io.{PrintWriter, File}

import ch.usi.inf.reveal.parsing.artifact.{StackOverflowUser, StackOverflowArtifact}
import ch.usi.inf.reveal.parsing.units.{TextReadabilityMetaInformation, InformationUnit}

/**
  * Created by luiscleto on 08/12/2015.
  */
class DiscussionAnalyser(filedir: String, filename: String, tagFilters: Seq[String]) {

  ///stores aggregated data for a discussion's answers
  class AnswersProperties(val max_score: Int, val avg_score: Double, val min_score: Int, val max_length: Int,
                          val avg_length: Double, val min_length: Int)
  class InformationUnitsProperties(val java_p: Double, val json_p: Double, val xml_p: Double, val stack_traces_p: Double,
                                   val total_length: Int, val words_count: Int, val intercalations: Int)

  val daysOfWeek = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  var numFiles = 0

  val pw_questions = new PrintWriter(filedir + "questions_" + filename)
  pw_questions.write("id," +
    "title length," +
    "tags count," +
    "tags popularity," + //TODO
    "java %," + //TODO
    "json %," + //TODO
    "xml %," + //TODO
    "stack traces %," + //TODO
    "length," + //TODO
    "words count," + //TODO
    "day of week," +
    "reputation," +
    "intercalations," + //TODO
    "score" +
    "number of answers" +
    "max answer score" + //TODO
    "avg answer score" + //TODO
    "min answer score" + //TODO
    "number of comments" +
    "max answer length" + //TODO
    "avg answer length" + //TODO
    "min answer length\n") //TODO

  val pw_answers = new PrintWriter(filedir + "answers_" + filename)
  pw_answers.write("question_id," + //TODO
    "id," + //TODO
    "first posted," +  //TODO
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

    var answersProperties: AnswersProperties = null

    if (artifact.answers.nonEmpty)
      answersProperties = processAnswers(artifact)

    pw_questions.println(Array(artifact.id.toString,
      artifact.question.title.length,
      artifact.question.tags.length,
      "TODO-Tag_Popularity",
      "TODO-java%",
      "TODO-json%",
      "TODO-xml%",
      "TODO-stack_traces%",
      "TODO-length",
      "TODO-words_count",
      daysOfWeek(artifact.question.creationDate.getDay),
      getOwnerReputation(artifact.question.owner),
      "TODO-intercalations",
      artifact.question.score,
      artifact.answers.length,
      if (answersProperties != null) answersProperties.max_score else "-",
      if (answersProperties != null) answersProperties.avg_score else "-",
      if (answersProperties != null) answersProperties.min_score else "-",
      artifact.question.comments.length,
      "TODO-max_answer_length",
      "TODO-avg_answer_length",
      "TODO-min_answer_length"
    ).mkString(","))
  }

  def processAnswers(artifact: StackOverflowArtifact): AnswersProperties = {
    val it = artifact.answers.iterator
    var maxAnswerScore = 0
    var avgAnswerScore = 0.0
    var minAnswerScore = Int.MaxValue
    var maxAnswerLength = 0
    var avgAnswerLength = 0.0
    var minAnswerLength = Int.MaxValue

    while (it.hasNext) {
      val answer = it.next()
      maxAnswerScore = Math.max(answer.score, maxAnswerScore)
      minAnswerScore = Math.min(answer.score, minAnswerScore)
      avgAnswerScore += answer.score

      val infIt = answer.informationUnits.iterator
      while (infIt.hasNext) {
        val informationUnit = infIt.next()
        val stf = informationUnit.toString()
        informationUnit.toString()
      }
    }
    avgAnswerScore /= artifact.answers.length

    //TODO write to answers file
    new AnswersProperties(maxAnswerScore, avgAnswerScore, minAnswerScore, maxAnswerLength, avgAnswerLength, minAnswerLength)
  }

  def processInformationUnits(informationUnits: Seq[InformationUnit]): InformationUnitsProperties = {
    var java_p: Double = 0
    var json_p: Double = 0
    var xml_p: Double = 0
    var stack_traces_p: Double = 0
    var total_length: Int = 0
    var words_count: Int = 0
    var intercalations: Int = 0

    val it = informationUnits.iterator

    while (it.hasNext) {
      val infUnit = it.next()
      total_length += infUnit.rawText.length

    }

    new InformationUnitsProperties(java_p, json_p, xml_p, stack_traces_p, total_length, words_count, intercalations)
  }

  def getOwnerReputation(owner: Option[StackOverflowUser]): String = {
    owner match {
      case Some(o) =>
        o.reputation.toString
      case None =>
        "No owner"
    }
  }
}

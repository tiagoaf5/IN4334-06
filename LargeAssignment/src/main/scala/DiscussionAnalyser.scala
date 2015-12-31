import java.io.{PrintWriter, File}

import ch.usi.inf.reveal.parsing.artifact.{StackOverflowUser, StackOverflowArtifact}
import ch.usi.inf.reveal.parsing.model.{TextFragmentNode, CommentNode, HASTNode}
import ch.usi.inf.reveal.parsing.model.java.JavaASTNode
import ch.usi.inf.reveal.parsing.model.json.JsonASTNode
import ch.usi.inf.reveal.parsing.model.stacktraces.StackTraceASTNode
import ch.usi.inf.reveal.parsing.model.xml.XmlASTNode
import ch.usi.inf.reveal.parsing.units.{NaturalLanguageTaggedUnit, CodeTaggedUnit, TextReadabilityMetaInformation, InformationUnit}

import scala.collection.mutable

/**
  * Created by luiscleto on 08/12/2015.
  */
class DiscussionAnalyser(filedir: String, filename: String, tagFilters: Seq[String]) {

  ///stores aggregated data for a discussion's answers
  class AnswersProperties(val max_score: Int, val avg_score: Double, val min_score: Int, val max_length: Int,
                          val avg_length: Double, val min_length: Int)
  class InformationUnitsProperties(val code_p: Double, val java_p: Double, val json_p: Double, val xml_p: Double, val stack_traces_p: Double,
                                   val total_length: Int, val words_count: Int, val intercalations: Int)

  val daysOfWeek = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  var numFiles = 0

  object CodeTypes extends Enumeration {
    type CodeType = Value
    val Java, XML, JSON, StackTrace, Undefined = Value
  }

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
    "max answer score" +
    "avg answer score" +
    "min answer score" +
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

      val iusProperties = processInformationUnits(answer.informationUnits)

    }
    avgAnswerScore /= artifact.answers.length

    //TODO write to answers file
    new AnswersProperties(maxAnswerScore, avgAnswerScore, minAnswerScore, maxAnswerLength, avgAnswerLength, minAnswerLength)
  }

  def processInformationUnits(informationUnits: Seq[InformationUnit]): InformationUnitsProperties = {
    var code_p: Double = 0
    var java_p: Double = 0
    var json_p: Double = 0
    var xml_p: Double = 0
    var stack_traces_p: Double = 0
    var total_length: Int = 0
    var words_count: Int = 0
    var intercalations: Int = 0

    val it = informationUnits.iterator

    var lastUnit: InformationUnit = null

    while (it.hasNext) {
      val infUnit = it.next()
      total_length += infUnit.rawText.length

      infUnit match {
        case u: CodeTaggedUnit =>
          if (lastUnit != null && lastUnit.isInstanceOf[NaturalLanguageTaggedUnit])
            intercalations += 1

          code_p += u.rawText.length

          var codeTypesFound: mutable.MutableList[CodeTypes.CodeType] = mutable.MutableList()

          u.astNode.fragments.foreach {
            case _: TextFragmentNode => println ("Text fragment in code ignored") //needs to be first since it derives from JavaASTNode
            case _: JavaASTNode => codeTypesFound += CodeTypes.Java
            case _: XmlASTNode => codeTypesFound += CodeTypes.XML
            case _: JsonASTNode => codeTypesFound += CodeTypes.JSON
            case _: StackTraceASTNode => codeTypesFound += CodeTypes.StackTrace
            case _: CommentNode => println ("Comment node ignored")
            case n => System.err.println ("Unidentified node found: " + n.toString)
          }

          if (codeTypesFound.toSet[CodeTypes.CodeType].size == 1) //check number of unique types
            codeTypesFound.head match {
              case CodeTypes.Java => java_p += infUnit.rawText.length
              case CodeTypes.XML => xml_p += infUnit.rawText.length
              case CodeTypes.JSON => json_p += infUnit.rawText.length
              case CodeTypes.StackTrace => stack_traces_p += infUnit.rawText.length
            }
          else
            System.err.println("Could not identify code type for fragment or found conflicting types")
        case _: NaturalLanguageTaggedUnit =>
          if (lastUnit != null && lastUnit.isInstanceOf[CodeTaggedUnit])
            intercalations += 1
          words_count += infUnit.rawText.split("\\W+").length
        case _ => ???
      }

      lastUnit = infUnit
    }
    code_p /= total_length
    java_p /= total_length
    json_p /= total_length
    xml_p /= total_length
    stack_traces_p /= total_length

    new InformationUnitsProperties(code_p, java_p, json_p, xml_p, stack_traces_p, total_length, words_count, intercalations)
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

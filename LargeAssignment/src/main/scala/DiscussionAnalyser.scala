import java.io.PrintWriter
import java.text.SimpleDateFormat

import ch.usi.inf.reveal.parsing.artifact.{StackOverflowComment, StackOverflowArtifact, StackOverflowUser}
import ch.usi.inf.reveal.parsing.model.java.JavaASTNode
import ch.usi.inf.reveal.parsing.model.json.JsonASTNode
import ch.usi.inf.reveal.parsing.model.stacktraces.StackTraceASTNode
import ch.usi.inf.reveal.parsing.model.xml.XmlASTNode
import ch.usi.inf.reveal.parsing.model.{CommentNode, TextFragmentNode}
import ch.usi.inf.reveal.parsing.units.{TextReadabilityMetaInformation, CodeTaggedUnit, InformationUnit, NaturalLanguageTaggedUnit}

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
  class CommentsProperties(val max_length: Int, val avg_length: Double, val min_length: Int)

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
    "max tag popularity (besides java)," +
    "avg tag popularity," +
    "min tag popularity," +
    "total code %," +
    "java %," +
    "json %," +
    "xml %," +
    "stack traces %," +
    "length," +
    "words count," +
    "Coleman-Liau Index," +
    "Flesch Reasing Ease Score," +
    "Flesch-Kincaid Grade Level," +
    "Automated Readability Index," +
    "Gunning Fog Index," +
    "SMOG Grade," +
    "day of week," +
    "reputation," +
//    "acceptance rate," + STORMED always returns NONE
    "intercalations," +
    "score," +
    "number of answers," +
    "max answer score," +
    "avg answer score," +
    "min answer score," +
    "number of comments," +
    "max answer length," +
    "avg answer length," +
    "min answer length," +
    "has accepted answer\n")

  val pw_answers = new PrintWriter(filedir + "answers_" + filename)
  pw_answers.write("question_id," +
    "id," +
    "first posted," +
    "same day as question," +
    "total code %," +
    "java %," +
    "json %," +
    "xml %," +
    "stack traces %," +
    "length," +
    "words count," +
    "Coleman-Liau Index," +
    "Flesch Reasing Ease Score," +
    "Flesch-Kincaid Grade Level," +
    "Automated Readability Index," +
    "Gunning Fog Index," +
    "SMOG Grade," +
    "day of week," +
    "reputation," +
//    "acceptance rate," + STORMED always returns NONE
    "intercalations," +
    "score," +
    "number of comments," +
    "accepted," +
    "max comment length," +
    "avg comment length," +
    "min comment length\n")

  def finish(): Unit = {
    pw_questions.close()
    pw_answers.close()

    System.out.println("Analyzed a total of " + numFiles + " discussions.")
  }

  def processDiscussion(artifact: StackOverflowArtifact): Unit = {

    if(!tagFilters.forall(artifact.question.tags.contains))
      return

    numFiles += 1
    val mis = artifact.question.metaInformation
    var answersProperties: AnswersProperties = null

    var textReadability: TextReadabilityMetaInformation = null

    for (mi <- artifact.question.metaInformation)
      mi match {
        case m:TextReadabilityMetaInformation => textReadability = m
        case _ =>
    }

    if (artifact.answers.nonEmpty)
      answersProperties = processAnswers(artifact)

    val iuProperties = processInformationUnits(artifact.question.informationUnits)

    var maxTagPop: Long = 0
    var minTagPop: Long = Long.MaxValue
    var avgTagPop: Double = 0.0

    for (tag <- artifact.question.tags) {
      if (tag != "java")
        maxTagPop = Math.max(maxTagPop, TagBank.getTagPopularity(tag))
      minTagPop = Math.min(minTagPop, TagBank.getTagPopularity(tag))
      avgTagPop += TagBank.getTagPopularity(tag)
    }
    avgTagPop /= artifact.question.tags.length
    
    //noinspection ScalaDeprecation
    pw_questions.println(Array(artifact.id.toString,
      artifact.question.title.length,
      artifact.question.tags.length,
      maxTagPop,
      avgTagPop,
      minTagPop,
      iuProperties.code_p,
      iuProperties.java_p,
      iuProperties.json_p,
      iuProperties.xml_p,
      iuProperties.stack_traces_p,
      iuProperties.total_length,
      iuProperties.words_count,
      if (textReadability != null) textReadability.colemanLiauIndex else "NA",
      if (textReadability != null) textReadability.fleshReadingEaseScore else "NA",
      if (textReadability != null) textReadability.fleshKincaidGradeLevel else "NA",
      if (textReadability != null) textReadability.automatedReadingIndex else "NA",
      if (textReadability != null) textReadability.gunningFogIndex else "NA",
      if (textReadability != null) textReadability.smogIndex else "NA",
      artifact.question.creationDate.getDay,
      getOwnerReputation(artifact.question.owner),
//      getOwnerAcceptanceRate(artifact.question.owner), STORMED always returns NONE
      iuProperties.intercalations,
      artifact.question.score,
      artifact.answers.length,
      if (answersProperties != null) answersProperties.max_score else "0",
      if (answersProperties != null) answersProperties.avg_score else "0",
      if (answersProperties != null) answersProperties.min_score else "0",
      artifact.question.comments.length,
      if (answersProperties != null) answersProperties.max_length else "0",
      if (answersProperties != null) answersProperties.avg_length else "0",
      if (answersProperties != null) answersProperties.min_length else "0",
      if (artifact.answers.exists(_.isAccepted)) "1" else "0"
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

    val answerIds = for (ans <- artifact.answers) yield ans.id

    val firstPostedId = answerIds.min

    while (it.hasNext) {
      val answer = it.next()
      maxAnswerScore = Math.max(answer.score, maxAnswerScore)
      minAnswerScore = Math.min(answer.score, minAnswerScore)
      avgAnswerScore += answer.score

      val iusProperties = processInformationUnits(answer.informationUnits)

      var textReadability: TextReadabilityMetaInformation = null

      for (mi <- answer.metaInformation)
        mi match {
          case m:TextReadabilityMetaInformation => textReadability = m
          case _ =>
        }

      maxAnswerLength = Math.max(iusProperties.total_length, maxAnswerLength)
      minAnswerLength = Math.min(iusProperties.total_length, minAnswerLength)
      avgAnswerLength += iusProperties.total_length

      var commentsProperties: CommentsProperties = null
      if (answer.comments.nonEmpty)
        commentsProperties = processComments(answer.comments)

      val fmt: SimpleDateFormat = new SimpleDateFormat("yyyyMMdd")
      val sameDay = fmt.format(answer.creationDate).equals(fmt.format(artifact.question.creationDate))

      //noinspection ScalaDeprecation
      pw_answers.println(Array(artifact.question.id,
        answer.id,
        if (answer.id == firstPostedId) 1 else 0,
        if (sameDay) 1 else 0,
        iusProperties.code_p,
        iusProperties.java_p,
        iusProperties.json_p,
        iusProperties.xml_p,
        iusProperties.stack_traces_p,
        iusProperties.total_length,
        iusProperties.words_count,
        if (textReadability != null) textReadability.colemanLiauIndex else "NA",
        if (textReadability != null) textReadability.fleshReadingEaseScore else "NA",
        if (textReadability != null) textReadability.fleshKincaidGradeLevel else "NA",
        if (textReadability != null) textReadability.automatedReadingIndex else "NA",
        if (textReadability != null) textReadability.gunningFogIndex else "NA",
        if (textReadability != null) textReadability.smogIndex else "NA",
        answer.creationDate.getDay,
        getOwnerReputation(answer.owner),
//        getOwnerAcceptanceRate(answer.owner), STORMED always returns NONE
        iusProperties.intercalations,
        answer.score,
        answer.comments.length,
        if (answer.isAccepted) 1 else 0,
        if (commentsProperties != null) commentsProperties.max_length else "0",
        if (commentsProperties != null) commentsProperties.avg_length else "0",
        if (commentsProperties != null) commentsProperties.min_length else "0"
      ).mkString(","))
    }
    avgAnswerScore /= artifact.answers.length
    avgAnswerLength /= artifact.answers.length


    new AnswersProperties(maxAnswerScore, avgAnswerScore, minAnswerScore, maxAnswerLength, avgAnswerLength, minAnswerLength)
  }

  def processComments(comments: Seq[StackOverflowComment] ): CommentsProperties = {
    val it = comments.iterator
    var max_length: Int = 0
    var min_length: Int = Int.MaxValue
    var avg_length: Double = 0

    while(it.hasNext) {
      val comment = it.next()
      val iuProps = processInformationUnits(comment.informationUnits)

      max_length = Math.max(iuProps.total_length, max_length)
      min_length = Math.min(iuProps.total_length, min_length)
      avg_length += iuProps.total_length
    }
    avg_length /= comments.length

    new CommentsProperties(max_length, avg_length, min_length)
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
            case _: TextFragmentNode => //println ("Text fragment in code ignored") //needs to be first since it derives from JavaASTNode
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
        case _ => throw new Exception("Unexpected Information Unit found")
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

  def getOwnerAcceptanceRate(owner: Option[StackOverflowUser]): String = {
    owner match {
      case Some(o) =>
        o.acceptRate match {
          case Some(ar) => ar.toString
          case None => "NA"
        }
      case None =>
        "NA"
    }
  }

  def getOwnerReputation(owner: Option[StackOverflowUser]): String = {
    owner match {
      case Some(o) =>
        o.reputation.toString
      case None =>
        "0"
    }
  }
}

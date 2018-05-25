import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import structures._

object BotAnswer {

  private val anonymityMap = Map(true -> "yes", false -> "no")
  private val visibilityMap = Map(true -> "continuous", false -> "afterstop")

  private val dateFormat = DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd")

  def viewPoll(p: Poll): String =
    s"""Poll "${p.name}":
       | Anonymity: ${anonymityMap(p.anonymity)}
       | Visibility: ${visibilityMap(p.visibility)}
       | Start time: ${date(p.startTime)}
       | Stop time: ${date(p.stopTime)}
       | Questions:
       |${p.question.zipWithIndex.map(q => viewQuestion(q._1, q._2)).mkString("\n")
    }
  """.stripMargin

  private def date(d: Option[LocalDateTime]): String =
    d match{
      case Some(x) => dateFormat.format(x)
      case None => "Not set"
    }

  private def viewQuestion(q: Question, id: Int): String = {
    val options =
      if (q.options.isEmpty)
        "Empty."
      else
        q.options.zipWithIndex.map(o => s"""    ${o._2}: "${o._1}"""").mkString("\n")
    s"""Question [$id]
       |   Name: "${q.name}"
       |   Type: ${qType(q)}
       |   Options:
       |$options
     """.stripMargin
  }

  private def qType(q: Question): String =
    q match {
      case _: OpenQuestion => "Open"
      case _: MultiQuestion => "Multi"
      case _: ChoiceQuestion => "Choice"
    }

  private def percent(q: ChoiceQuestion, id: Int): Int =
    (q.answers.count(_._2 == id) / q.answered.length.toFloat * 100).toInt

  private def percent(q: MultiQuestion, id: Int): Int =
    (q.answers.count(_._2.contains(id)) / q.answered.length.toFloat * 100).toInt

  private def percent(q: OpenQuestion, answer: String): Int =
    (q.answers.count(_._2 == answer) / q.answered.length.toFloat * 100).toInt

  def choiceResult(q: ChoiceQuestion): String =
    s"""${q.options.zipWithIndex.map(i =>
      "    \"" + i._1 + "\": " + percent(q, i._2) + "%").mkString("\n")}""".stripMargin

  def multiResult(q: MultiQuestion): String =
    s"""${q.options.zipWithIndex.map(i =>
      "    \"" + i._1 + "\": " + percent(q, i._2) + "%").mkString("\n")}""".stripMargin

  def openResult(q: OpenQuestion): String =
    s"""${q.answers.map(_._2).toSet[String].map(s =>
      "    \"" + s + "\": " + percent(q, s) + "%").mkString("\n")}""".stripMargin

  def viewResult(p: Poll): String =
    s"""Poll "${p.name}":
       |${p.question.map(questionResult).mkString("\n")}""".stripMargin

  def questionResult(q: Question): String =   s"""  "${q.name}" [${qType(q)}]:\n""" +
    (q match {
      case x: ChoiceQuestion => choiceResult(x)
      case x: MultiQuestion => multiResult(x)
      case x: OpenQuestion => openResult(x)
    })

}

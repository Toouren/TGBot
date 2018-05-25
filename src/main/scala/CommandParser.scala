import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.Try
import Commands._
import structures.Types._

import scala.util.parsing.combinator._


trait CommandParser {
  def parse(string: String): Try[Command]
}

object CommandParser extends RegexParsers with CommandParser {
  private val anonymityMap = Map("yes" -> true, "no" -> false)
  private val visibilityMap = Map("afterstop" -> false, "continuous" -> true)

  def leftBracket: Parser[String] = "((" ^^^ "("

  def rightBracket: Parser[String] = "))" ^^^ ")"

  def string: Parser[String] = """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!?]+""".r

  def digits: Parser[Int] = """\d+""".r ^^ (_.toInt)

  def date: Parser[Option[LocalDateTime]] =
    """(\d{2}:){2}\d{2} (\d{2}:){2}\d{2}""".r ^^ {
      date => Try(LocalDateTime.parse(date, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))).toOption
    }

  def anonymity: Parser[Boolean] = "(" ~> "yes|no".r <~ ")" ^^ (anonymityMap(_))

  def visibility: Parser[Boolean] = "(" ~> "afterstop|continuous".r <~ ")" ^^ (visibilityMap(_))

  def stringArgument: Parser[String] =
    "(" ~> rep(leftBracket | rightBracket | string) <~ ")" ^^ (xs => xs.mkString(sep = " "))

  def digitArgument: Parser[Int] = "(" ~> digits <~ ")"

  def dateArgument: Parser[Option[LocalDateTime]] = "(" ~> date <~ ")"

  def createPoll: Parser[CreatePoll] =
    "/create_poll" ~> stringArgument ~
      opt(
        anonymity ~
          opt(visibility ~
            opt(dateArgument ~
              opt(dateArgument)
            )
          )
      ) ^^ {
      case name ~ None =>
        CreatePoll(name)
      case name ~ Some(anonymity ~ None) =>
        CreatePoll(name, anonymity)
      case name ~ Some(anonymity ~ Some(visibility ~ None)) =>
        CreatePoll(name, anonymity, visibility)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ None))) =>
        CreatePoll(name, anonymity, visibility, startTime)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ Some(endTime)))) =>
        CreatePoll(name, anonymity, visibility, startTime, endTime)
    }

  def list: Parser[List_] = "/list".r ^^^ List_()

  def deletePoll: Parser[DeletePoll] = "/delete_poll".r ~> digitArgument ^^ { id: Int => DeletePoll(id)}

  def startPoll: Parser[StartPoll] = "/start_poll".r ~> digitArgument ^^ { id: Int => StartPoll(id) }

  def stopPoll: Parser[StopPoll] = "/stop_poll".r ~> digitArgument ^^ { id: Int => StopPoll(id)}

  def result: Parser[Result] = "/result".r ~> digitArgument ^^ { id: Int => Result(id)}

  def begin: Parser[Begin] = "/begin".r ~> digitArgument ^^ { id: Int => Begin(id)}

  def end: Parser[End] = "/end".r ^^^ End()

  def view: Parser[View] = "/view".r ^^^ View()

  private def qType: Parser[QuestionType] = opt("(" ~> "open|multi|choice".r <~ ")") ^^ {
    case Some(s) => s match {
      case "open" => Open
      case "choice" => Choice
      case "multi" => Multi
    }
    case None => Open
  }

  def addQuestion: Parser[AddQuestion] = "/add_question".r ~>
    stringArgument ~
    qType ~
    rep(stringArgument) ^? {
    case name ~ Open ~ options if options.isEmpty => AddQuestion(name, Nil)
    case name ~ Multi ~ options if options.nonEmpty => AddQuestion(name, options, Multi)
    case name ~ Choice ~ options if options.nonEmpty => AddQuestion(name, options, Choice)
  }

  def deleteQuestion: Parser[DeleteQuestion] = "/delete_question".r ~> digitArgument ^^ {
    id: Int => DeleteQuestion(id)
  }

  def answer: Parser[Answer] = "/answer".r ~> digitArgument ~ stringArgument ^^ {
    case id ~ answer => Answer(id, answer)
  }

  def command: Parser[Command] = {
    createPoll | list | deletePoll | startPoll | stopPoll | result | begin | end | view |
      addQuestion | deleteQuestion | answer
  }

  override def parse(string: String): Try[Command] = {
    Try {
      CommandParser.parseAll(command, string) match {
        case Success(r, _) => r
        case Failure(m, _) => throw new IllegalArgumentException(s"$m\n$string")
      }
    }
  }
}
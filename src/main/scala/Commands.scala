import java.time.LocalDateTime

import structures.Types.{Open, QuestionType}

package object Commands {

  sealed trait Command

  case class CreatePoll(name: String,
                        anonymity: Boolean = true,
                        visibility: Boolean = true,
                        startTime: Option[LocalDateTime] = None,
                        stopTime: Option[LocalDateTime] = None) extends Command
  case class List_() extends Command
  case class DeletePoll(id: Int) extends Command
  case class StartPoll(id: Int) extends Command
  case class StopPoll(id: Int) extends Command
  case class Result(id: Int) extends Command

  case class Begin(id: Int) extends Command
  case class End() extends Command
  case class View() extends Command
  case class AddQuestion(question: String, variant: List[String],
                         qType: QuestionType = Open) extends Command

  case class DeleteQuestion(id: Int) extends Command
  case class Answer(id: Int, answers: String) extends Command
}

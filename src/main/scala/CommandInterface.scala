import java.time.LocalDateTime

trait Command

case class CreatePoll(name: String, anonymity: Boolean = true, visibility: Boolean = false,
                      startTime: Option[LocalDateTime] = None, endTime: Option[LocalDateTime] = None) extends Command

case class List() extends Command

case class DeletePoll(id: Int) extends Command

case class StartPoll(id: Int) extends Command

case class StopPoll(id: Int) extends Command

case class Result(id: Int) extends Command

case class BadCommand(massage: String) extends Command
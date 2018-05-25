import java.time.LocalDateTime

object CommandExecutor {
  var memoryPoll: Map[Int, Poll] = Map.empty
  private val idGenerator = Stream.from(1).iterator

  private def executeCommand(command: Command): String = {
    command match {
      case CreatePoll(name, anonymity, visibility, startTime, endTime) =>
        val id = idGenerator.next()
        memoryPoll += (id -> new Poll(name, anonymity, visibility, startTime, endTime))
        s"Poll with ($id) id was created"

      case List() => memoryPoll.mkString("\n")

      case DeletePoll(id) => memoryPoll.get(id) match {
        case Some(poll) =>
          memoryPoll -= id
          s"Poll with ($id) was deleted"
        case None => s"Poll with ($id) id not found"
      }

      case StartPoll(id) => memoryPoll.get(id) match {
        case Some(poll) =>
          s"Poll with ($id) id was started"
        case None => s"Poll with ($id) id not found"
      }

      case StopPoll(id) => memoryPoll.get(id) match {
        case Some(poll) =>
          s"Poll with ($id) id was stop"
        case None => s"Poll with ($id) id not found"
      }

      case Result(id) => memoryPoll.get(id) match {
        case Some(poll) =>
          poll.toString()
        case None => s"Poll with ($id) id not found"
      }

      case BadCommand(massage) => massage
    }
  }

  def applyCommand(command: Command): String = executeCommand(command)
}

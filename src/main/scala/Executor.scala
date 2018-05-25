import java.time.LocalDateTime

import structures._
import Commands._
import structures.Types._

import scala.util.Try

object Executor {
  var memoryPoll: Map[Int, Poll] = Map.empty
  var currentPoll: Map[User, Int] = Map.empty
  val idStream: Iterator[Int] = Stream.from(1).iterator


  def executeCommand(user: User, command: Command): String = {
    command match {
      case x: CreatePoll => createPoll(user, x)
      case x: DeletePoll => deletePoll(user, x)
      case x: List_ => list(user, x)
      case x: StartPoll => startPoll(user, x)
      case x: StopPoll => stopPoll(user, x)
      case x: Result => result(user, x)
      case x: Begin => begin(user, x)
      case x: End => end(user, x)
      case x: View => view(user, x)
      case x: AddQuestion => addQuestion(user, x)
      case x: DeleteQuestion => deleteQuestion(user, x)
      case x: Answer => answer(user, x)
    }
  }

  def createPoll(user: User, command: CreatePoll): String = {
    val id = idStream.next()
    memoryPoll = memoryPoll + (id -> Poll(command.name, command.anonymity,
      command.visibility, command.startTime, command.stopTime, user, Vector.empty))
    s"Poll with ($id) id was created."
  }

  def deletePoll(user: User, command: DeletePoll): String = {
    memoryPoll.get(command.id) match {
      case Some(_) =>
        memoryPoll -= command.id
        s"Poll with (${command.id}) id was deleted."
      case None => s"Poll with (${command.id}) id not found."
    }
  }

  def list(user: User, command: List_): String = {
    val list = memoryPoll.toList.map(poll => poll._1 + ": \"" + poll._2.name + "\"")
    if (list.nonEmpty) list.mkString("\n") else "Poll list is empty."
  }

  def startPoll(user: User, command: StartPoll): String = {
    memoryPoll.get(command.id) match {
      case Some(poll) =>
        if (poll.owner != user) s"You have not rights to start poll with (${command.id})."
        else if (Poll.isStarted(poll, LocalDateTime.now())) s"Poll with (${command.id}) already started."
        else {
          poll.startTime match {
            case Some(_) => s"Poll with (${command.id}) cannot be start."
            case None =>
              memoryPoll = memoryPoll updated(command.id, poll.copy(startTime = Option(LocalDateTime.now)))
              s"Poll with (${command.id}) was started."
          }
        }
      case None => s"Poll with (${command.id}) id not found."
    }
  }

  def stopPoll(user: User, command: StopPoll): String = {
    memoryPoll.get(command.id) match {
      case Some(poll) =>
        if (poll.owner != user) s"You have not rights to stop poll with (${command.id})."
        else if (Poll.isStopped(poll, LocalDateTime.now())) s"Poll with (${command.id}) already stopped."
        else {
          poll.stopTime match {
            case Some(_) => s"Poll with (${command.id}) cannot be stop."
            case None =>
              memoryPoll = memoryPoll updated(command.id, poll.copy(stopTime = Option(LocalDateTime.now)))
              s"Poll with (${command.id}) id was stopped."
          }
        }
      case None => s"Poll with (${command.id}) id not found."
    }
  }

  def result(user: User, command: Result): String = {
    memoryPoll.get(command.id).map(poll => {
      if (Poll.isStarted(poll, LocalDateTime.now()) && !poll.visibility) "You cannot get information now"
      else BotAnswer.viewResult(poll)
    }).getOrElse( s"Poll with (${command.id}) id not found.")
  }

  //Command with context

  def begin(user: User, command: Begin): String = {
    if (currentPoll.get(user).isDefined) "You are already in context."
    else {
      memoryPoll.get(command.id) match {
        case Some(poll) =>
          if (!Poll.isStopped(poll, LocalDateTime.now())) {
            currentPoll += (user -> command.id)
            s"You start work with poll with (${command.id}) id."
          } else s"You cannot start work with poll with (${command.id}) id."
        case None => s"Poll with (${command.id}) id not found."
      }
    }
  }

  def end(user: User, command: End): String = {
    currentPoll.get(user) match {
      case None => "You are not in context."
      case Some(pollId) =>
        currentPoll -= user
        s"You stop work with poll with ($pollId) id."
    }
  }

  def view(user: User, command: View): String = {
    currentPoll.get(user).map(pollId => {
      memoryPoll.get(pollId).map(poll => {
        BotAnswer.viewPoll(poll)
    }).getOrElse(s"Poll with ($pollId) id not found.")
      }).getOrElse("You are not in context.")
  }

  def addQuestion(user: User, command: AddQuestion): String = {
    currentPoll.get(user).map(pollId => {
      memoryPoll.get(pollId).map(poll => {
        if (poll.owner == user) {
          if (Poll.isStopped(poll, LocalDateTime.now())) "Poll already stopped."
          else if (Poll.isStarted(poll, LocalDateTime.now())) "Poll already start."
          else command.qType match {
            case Open =>
              if (command.variant.nonEmpty) "Some options in question. Expected 0."
              else {
                memoryPoll = memoryPoll updated(pollId, poll.copy(question = poll.question :+ OpenQuestion(command.question)))
                s"Question №${poll.question.length} was added."
              }
            case Choice =>
              if (command.variant.isEmpty || command.variant.length < 2) "Few options in question. Expected more then 1."
              else {
                memoryPoll = memoryPoll updated(pollId,
                  poll.copy(question = poll.question :+ ChoiceQuestion(command.question, options = command.variant)))
                s"Question №${poll.question.length} was added."
              }
            case Multi =>
              if (command.variant.isEmpty || command.variant.length < 2) "Few options in question. Expected more than 1."
              else {
                memoryPoll = memoryPoll updated(pollId,
                  poll.copy(question = poll.question :+ MultiQuestion(command.question, options = command.variant)))
                s"Question №${poll.question.length} was added."
              }
          }
        }
        else "You cannot add question in this poll"
      }).getOrElse(s"Poll with ($pollId) id not found.")
    }).getOrElse("You are not in context.")
  }

  def deleteQuestion(user: User, commands: DeleteQuestion): String = {
    currentPoll.get(user).map(pollId => {
      memoryPoll.get(pollId).map(poll => {
        if (poll.owner == user) {
          if (poll.question.lengthCompare(commands.id) <= 0) s"Question with ${commands.id} number does not exist"
          else {
            if (Poll.isStopped(poll, LocalDateTime.now())) "Poll already stopped."
            else if (Poll.isStarted(poll, LocalDateTime.now())) "Poll already start."
            else {
              memoryPoll = memoryPoll updated(pollId, poll.copy(question = poll.question.patch(commands.id, Nil, 1)))
              s"Question with ${commands.id} number deleted."
            }
          }
        }
        else "You cannot delete question in this poll"
      }).getOrElse(s"Poll with ($pollId) id not found.")
    }).getOrElse("You are not in context.")
  }

  def answer(user: User, command: Answer): String = {
    currentPoll.get(user).map(pollId => {
      memoryPoll.get(pollId).map(poll => {
        if (poll.question.lengthCompare(command.id) <= 0) s"Question with ${command.id} number does not exist"
        else {
          if (Poll.isStopped(poll, LocalDateTime.now())) "Poll are already stooped"
          else if (!Poll.isStarted(poll, LocalDateTime.now())) "Poll are not start"
          else if (poll.question(command.id).answered.contains(user)) "You already answer on it question."
          else {
            def setter(newQuestion: Question): Unit = memoryPoll =
              memoryPoll updated(pollId, poll.copy(question = poll.question.patch(command.id, Seq(newQuestion), 1)))

            poll.question(command.id) match {
              case x: OpenQuestion => answerOpen(user, command.answers, poll.anonymity, x, setter)
              case x: ChoiceQuestion => answerChoice(user, command.answers, poll.anonymity, x, setter)
              case x: MultiQuestion => answerMulti(user, command.answers, poll.anonymity, x, setter)
            }
          }
        }
      }).getOrElse(s"Poll with ($pollId) id not found.")
    }).getOrElse("You are not in context.")
  }

  private def answerOpen(user: User, answer: String, anon: Boolean,
                         question: OpenQuestion, f: OpenQuestion => Unit): String = {
    f(question.answer(user, answer, anon))
    "Answer submitted."
  }

  private def answerChoice(user: User, answer: String, anon: Boolean,
                           question: ChoiceQuestion, f: ChoiceQuestion => Unit): String = {
    Try(answer.toInt - 1).toOption match {
      case Some(ans) =>
        if (question.options.isDefinedAt(ans)) {
          f(question.answer(user, ans, anon))
          "Answer submitted."
        }
        else "Option index is out of range."
      case None => "Incorrect answer format."
    }
  }

  private def answerMulti(user: User, answer: String, anon: Boolean,
                          question: MultiQuestion, f: MultiQuestion => Unit): String = {
    Try(answer.split(" ").map(_.toInt - 1)).toOption match {
      case Some(ans) =>
        if (ans.distinct.lengthCompare(ans.length) != 0) "Option indices must be distinct."
        else if (!ans.forall(question.options.isDefinedAt)) "Option index is out of range."
        else {
          f(question.answer(user, ans.toSet, anon))
          "Answer submitted."
        }
      case None => "Incorrect answer format."
    }
  }
}

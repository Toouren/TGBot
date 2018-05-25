package structures

import java.time.LocalDateTime


case class Poll(name: String, anonymity: Boolean, visibility: Boolean,
                startTime: Option[LocalDateTime], stopTime: Option[LocalDateTime],
                owner: Long, question: Vector[Question])


object Poll {

  def isStarted(poll: Poll, time: LocalDateTime): Boolean =
    poll.startTime.exists(time.isAfter)

  def isStopped(poll: Poll, time: LocalDateTime): Boolean  =
    poll.stopTime.exists(time.isAfter)

  def isActive(poll: Poll, time: LocalDateTime): Boolean =
    isStarted(poll, time) && !isStopped(poll, time)
}
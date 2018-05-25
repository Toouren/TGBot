import java.time.LocalDateTime

class Poll(name: String, anonymity: Boolean, visibility: Boolean,
           startTime: Option[LocalDateTime], endTime: Option[LocalDateTime]) {

  override def toString: String = {
    s"""Name:  $name
       |Anonymity:  $anonymity
       |Visibility:  $visibility
       |Start Time:  ${startTime.getOrElse("Start time not set")}
       |End Time:  ${endTime.getOrElse("End time not set")}""".stripMargin
  }
}
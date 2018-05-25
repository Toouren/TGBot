package structures

object Types {
  type User = Long

  sealed trait QuestionType
  case object Open extends QuestionType
  case object Choice extends QuestionType
  case object Multi extends QuestionType

  type OpenAnswer = (Option[User], String)
  type ChoiceAnswer = (Option[User], Int)
  type MultiAnswer = (Option[User], Set[Int])

}

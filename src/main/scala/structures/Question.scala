package structures

import structures.Types._

sealed trait Question {
  val name: String
  val options: List[String]
  val answered: List[User]
}

final case class OpenQuestion(name: String,
                              answers: List[OpenAnswer] = Nil,
                              answered: List[User] = Nil,
                              options: List[String] = Nil
                             ) extends Question {
  def answer(user: User, answer: String, anon: Boolean): OpenQuestion =
    copy(answered = answered :+ user, answers = answers :+ (if (anon) None else Some(user), answer))
}


final case class ChoiceQuestion(name: String,
                                answers: List[ChoiceAnswer] = Nil,
                                answered: List[User] = Nil,
                                options: List[String] = Nil
                             ) extends Question {
  def answer(user: User, answer: Int, anon: Boolean): ChoiceQuestion =
    copy(answered = answered :+ user, answers = answers :+ (if (anon) None else Some(user), answer))
}

final case class MultiQuestion(name: String,
                               answers: List[MultiAnswer] = Nil,
                               answered: List[User] = Nil,
                               options: List[String] = Nil
                             ) extends Question {
  def answer(user: User, answer: Set[Int], anon: Boolean): MultiQuestion =
    copy(answered = answered :+ user, answers = answers :+ (if (anon) None else Some(user), answer))
}

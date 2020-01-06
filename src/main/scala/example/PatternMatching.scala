package example

object PatternMatching {
  case class Pet(name: String, says: String)

  val catSaysRx = "^(meow|nya)$".r
  val robotSaysRx = "^([01]+)$".r

  def kind(pet: Pet): String = pet match {
    case Pet(_, catSaysRx(_)) => "cat"
    case Pet("Rex", _) => "dog"
    case Pet(_, robotSaysRx(_)) => "robot"
    case _ => "unknown"
  }
}

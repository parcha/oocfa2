sealed abstract class Option[A]
final case class Some[+A] (x: A) extends Option[A]
case object None extends Option[Nothing]


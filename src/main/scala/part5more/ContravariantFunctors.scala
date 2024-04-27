package part5more

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A) = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String) = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int) = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean) = if (value) "Y" else "N"
  }

  implicit def getOptionFormat[T](implicit
      f: Format[T],
      m: Monoid[T]
  ): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] =
    Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far."))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
    import cats.instances.option._
    println(format(Option(Option(42))))
  }
}

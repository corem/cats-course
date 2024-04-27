package part4typeclasses

object Applicatives {
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(3)

  import cats.instances.option._
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(3)

  import cats.syntax.applicative._
  val aSweetList = 2.pure[List]
  val aSweetOption = 2.pure[Option]

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)
  val validatedApplicative = Applicative[ErrorsOr]

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit
      applicative: Applicative[W]
  ): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] =
      applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  def main(args: Array[String]): Unit = {}
}

package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(
    Some(123),
    Some("a string")
  ) // Some((123,"a string"))

  import cats.instances.future._
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val aTupledFuture =
    Semigroupal[Future].product(Future("Meaning of life"), Future(42))

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit
      monad: Monad[F]
  ): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal =
    Semigroupal[ErrorsOr]
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product(
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]) =
      listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
  }
}

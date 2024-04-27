package part4typeclasses

import java.util.concurrent.Executors

import cats.{Applicative, Monad}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] =
      handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32)
  val failure = monadErrorEither.raiseError[Int](
    "Something wrong"
  )

  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _         => 89
  }

  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44)
    case _         => Left("Something else")
  }

  val filteredSuccess =
    monadErrorEither.ensure(success)("Number too small")(_ > 100)

  import cats.instances.try_._
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] =
    MonadError[Try, Throwable].raiseError(exception)
  import cats.instances.future._
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(
    exception
  )

  import cats.data.Validated
  import cats.instances.list._
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val extendedSuccess =
    42.pure[
      ErrorsOr
    ]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover { case _ =>
    43
  }

  import cats.syntax.monadError._
  val testedSuccess = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {}
}

package part2abstractMath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List]
  val aSimpleList = monadList.pure(2)
  val aExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // Applicable to Option, Try, Future

  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life...")
  )

  val errorOrMonad = Monad[ErrorOr]

  def main(args: Array[String]): Unit = {}
}

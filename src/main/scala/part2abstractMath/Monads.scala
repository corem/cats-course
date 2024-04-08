package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  val combinationsList = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  val numberOption = Option(2)
  val charOption = Option('a')

  val combinationsOption =
    numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationsOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val numberFuture = Future(42)
  val charFuture = Future('a')

  val combinationsFuture =
    numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationsFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  import cats.Monad
  import cats.instances.option._ // Implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption =
    optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList =
    listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4,5)

  import cats.instances.future._
  val futureMonad = Monad[Future] // Requires an implicit ExecutionContext
  val aFuture = futureMonad.pure(43)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x =>
    Future(x + 44)
  ) // Future that will end up with a Success(87)

  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(
      number: Option[Int],
      char: Option[Char]
  ): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(
      number: Future[Int],
      char: Future[Char]
  ): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // Generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // Extension methods
  import cats.syntax.applicative._ // Pure is here
  val oneOption = 1.pure[Option] // Implicit Monad[Option] will be use
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // Monads extend Functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor._ // map is here
  val oneOptionMapped2 = oneOption.map(_ + 2)
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  def getPairsFor[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] = {
    for {
      a <- ma
      b <- mb
    } yield (a, b) // Same as ma.flatMap(a => mb.map(b => (a, b)))
  }

  def main(args: Array[String]): Unit = {
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, numberOption))
    getPairs(numberFuture, charFuture).foreach(println)
  }
}

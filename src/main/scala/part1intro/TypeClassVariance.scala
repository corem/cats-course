package part1intro

object TypeClassVariance {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None

  class Animal
  class Cat extends Animal

  // Covariant type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]

  // Contravariant type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal]
}

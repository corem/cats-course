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

  // Rule of thumb:
  // "HAS a T" = covariant
  // "ACTS on T" = contravariant

  // Covariant type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]

  // Contravariant type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal]

  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("Wow")
  makeSound[Animal] // Ok, TC instances defined above
  makeSound[Cat] // Ok, TC instance for Animal is also applicable to Cats

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animals everywhere"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "So many cats"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat])
    // println(organizeShow[Animal]) // Compilation KO, ambiguous values
  }
}

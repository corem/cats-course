package part1intro

object CatsIntro {

  // Eq
  val aComparison = 2 == "a string"

  // Type class import
  import cats.Eq
  // Type class instances for specific type
  import cats.instances.int._
  // Type class API
  val intEquality = Eq[Int]

  val aTypeSafeComparison = intEquality.eqv(2, 3)
  // val anUnsafeComparison = intEquality.eqv(2, "string")

  // Extensions method
  import cats.syntax.eq._
  val anotherTypeSafeComparison = 2 === 3
  val neqComparison = 2 =!= 3
  // val anOtherUnsafeComparison = 2 === "string"

  import cats.instances.list._
  val aListComparison = List(2) === List(3)
  import cats.instances.string._
  val anOtherListComparison = List("Test") === List("Test")

  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lambo", 29.99)
}

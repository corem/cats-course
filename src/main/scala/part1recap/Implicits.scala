package part1recap

object Implicits {

  // Implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name !"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  val impersonableString = new ImpersonableString("Peter")
  impersonableString.greet

  val greeting = "Peter".greet

  // Importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // Implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10
  val incremented2 = increment(2)

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)

  def main(args: Array[String]): Unit = {}
}

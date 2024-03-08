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

  // More complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJSon[T](list: List[T])(implicit JSONSerializer: JSONSerializer[T]): String = {
    list.map(value => JSONSerializer.toJson(value)).mkString("[", ",", "]")
  }

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name" : "${person.name}"}
      |""".stripMargin
  }
  val personJson = listToJSon(List(Person("Alice"), Person("Bob")))

  def main(args: Array[String]): Unit = {}
}

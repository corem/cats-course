package part2abstractMath

object Semigroups {
  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // Addition

  import cats.instances.string._

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // Concatenation

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup = Semigroup.instance[Expense] {
    (e1, e2) => Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // Extension methods for Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3
  val anStringSum = "2" |+| "3"
  val aCombinedExpense = Expense(1, 10) |+| Expense(2, 23)

  def reduceThings2[T: Semigroup](list: List[T]): T =
    list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("I'm ","starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings))

    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option._
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // An Option[Int] containing the sum of all the numbers

    val expenses = List(Expense(1, 99), Expense(2, 35), Expense(3, 10), Expense(4, 12))
    println(reduceThings(expenses))
    println(reduceThings2(expenses))
  }
}

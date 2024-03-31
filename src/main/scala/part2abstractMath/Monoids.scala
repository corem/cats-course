package part2abstractMath

object Monoids {
  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers = (1 to 1000).toList

  val sumLeft = numbers.foldLeft(0)(_ + _)
  val sumRight = numbers.foldRight(0)(_ + _)

//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//    list.foldLeft(/* Semigroup is not enough to define this starting value*/)(_ |+| _)

  // Monoids
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(2, 3)
  val zero = intMonoid.empty // 0

  import cats.instances.string._
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("Hello ", "Cats")

  import cats.instances.option._ // Construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty

  val combineOption =
    Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // Extension methods for Monoids
  val combinedOptionFancy = Option(3) |+| Option(7)

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  val phonebooks = List(
    Map("Alice" -> 234, "Bob" -> 657),
    Map("Charlie" -> 244, "Daniel" -> 123),
    Map("Tina" -> 126)
  )

  import cats.instances.map._
  val mergedPhonebook = combineFold(phonebooks)

  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
      ShoppingCart(List(), 0.0),
      (sa, sb) => ShoppingCart(sa.items ++ sb.items, sa.total + sb.total)
    )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = {
    combineFold(shoppingCarts)
  }

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "Monoids")))
    println(mergedPhonebook)
    val shoppingCarts = List(
      ShoppingCart(List("iPhone", "Shoes"), 799.0),
      ShoppingCart(List("TV"), 2999.0),
      ShoppingCart(List(), 0)
    )
    println(checkout(shoppingCarts))
  }
}

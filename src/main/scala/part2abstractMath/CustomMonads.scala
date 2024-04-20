package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(v))  => tailRecM(v)(f)
        case Some(Right(v)) => Some(v)
      }
  }

  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonde extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(
        f: A => Identity[B]
    ): Identity[B] =
      f(fa)

    @tailrec
    override def tailRecM[A, B](
        a: A
    )(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => v
    }
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(v)             => f(v)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]) = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v))       => stackRec(f(v))
        case Leaf(Right(b))      => Leaf(b)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      @tailrec
      def tailRec(
          todo: List[Tree[Either[A, B]]],
          expanded: List[Tree[Either[A, B]]],
          done: List[Tree[B]]
      ): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(v))  => tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
            case node @ Branch(left, right) =>
              if (expanded.isEmpty || node != expanded.head) {
                tailRec(right :: left :: todo, node :: expanded, done)
              } else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded.tail, newBranch :: done.drop(2))
              }
          }

      // stackRec(f(a))
      tailRec(List(f(a)), List(), List())
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree =
      TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))

    println(changedTree)

    val example: Tree[Either[Int, String]] =
      Branch(
        Branch(Leaf(Left(1)), Leaf(Left(2))),
        Branch(Leaf(Left(1)), Leaf(Left(2)))
      )
    def fun(x: Int): Tree[Either[Int, String]] =
      if (x == 0) example
      else Leaf(Right((x * 10).toString))

    print(Monad[Tree].tailRecM(0)(fun))
  }
}

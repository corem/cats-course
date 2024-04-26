package part3datamanipulation

object Evaluation {

  import cats.Eval
  val instantEval = Eval.now {
    println("Computing now!")
    64365
  }

  val redoEval = Eval.always {
    println("Computing again!")
    45454
  }

  val memoEval = Eval.later {
    println("Computing later!")
    45454
  }

  val composedEvaluation =
    instantEval.flatMap(value1 => memoEval.map(value2 => value1 + value2))
  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- memoEval
  } yield value1 + value2

  val evalEx1 = for {
    a <- memoEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always { println("Step 1..."); "Put the guitar on your lap" }
    .map { step1 =>
      println("Step 2"); s"$step1 then put your left hand on the neck"
    }
    .memoize
    .map { steps12 =>
      println("Step 3, more complicated");
      s"$steps12 then with the right hand strike the strings"
    }

  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    // println(instantEval.value)
    // println(redoEval.value)
    // println(composedEvaluation.value)

    println(defer(Eval.now {
      println("Now!")
      42
    }).value)

    println(reverseEval((1 to 10000).toList).value)
  }
}

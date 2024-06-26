package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._
  import cats.instances.future._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(
    List(Option(1), Option(2))
  )
  val listOfCharOptions: OptionT[List, Char] = OptionT(
    List(Option('a'), Option('b'), Option.empty[Char])
  )

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(
    List(Left("Something Wrong"), Right(43), Right(2))
  )

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  val bandwiths = Map(
    "server1.corem.com" -> 50,
    "server2.corem.com" -> 300,
    "server3.corem.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwiths.get(server) match {
      case None =>
        EitherT.left(Future(s"Server $server unreachable"))
      case Some(b) => EitherT.right(Future(b))
    }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250

  def generateTrafficSpikeReport(
      s1: String,
      s2: String
  ): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(reason) =>
        Left(
          s"Servers $s1 and $s2 cannot cope with the incoming spike: $reason"
        )
      case Right(false) =>
        Left(
          s"Servers $s1 and $s2 cannot cope with the incoming spike: not enough total bandwidth"
        )
      case Right(true) =>
        Right(
          s"Servers $s1 and $s2 can cope with the incoming spike"
        )
    }
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    val resultFuture = generateTrafficSpikeReport(
      "server1.corem.com",
      "server2.corem.com"
    ).value
    resultFuture.foreach(println)
  }
}

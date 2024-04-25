package part3datamanipulation

object Readers {

  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "Dispatched"
    def getLastOrderId(username: String): Long = 123123
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started")
  }

  val config =
    Configuration("username", "password", "host", 8080, 8, "corem@core.com")
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(config => DbConnection(config.dbUsername, config.dbPassword))
  val dbConn = dbReader.run(config)

  val userOrderStatusReader: Reader[Configuration, String] =
    dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val userOrderStatus = userOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val usersLastOrderIdReaderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersLastOrderIdReaderFor.run(config)
  }

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, content: String) = {
      s"From: $emailReplyTo; to: $address >>> $content"
    }
  }

  def emailUser(username: String, userEmail: String) = {
    val emailServiceReader: Reader[Configuration, EmailService] =
      Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(
      userEmail,
      s"Your last order has the status: $orderStatus"
    )

    emailReader.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("test"))
    println(emailUser("corem", "corem2@core.com"))
  }
}

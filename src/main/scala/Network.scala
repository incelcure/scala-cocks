import java.net._
import scala.concurrent.{Future, ExecutionContext}
import scala.util.Try

object Network {
  implicit val ec: ExecutionContext = ExecutionContext.global

  def startServer(port: Int)(handler: String => String): Unit = Future {
    val server = new ServerSocket(port)
    while(true) {
      val client = server.accept()
      Future {
        val in = new java.io.BufferedReader(new java.io.InputStreamReader(client.getInputStream))
        val out = new java.io.PrintWriter(client.getOutputStream, true)
        val request = in.readLine()
        val response = handler(request)
        out.println(response)
        client.close()
      }
    }
  }

  def sendMessage(host: String, port: Int, message: String): Future[String] = Future {
    val socket = new Socket(host, port)
    val out = new java.io.PrintWriter(socket.getOutputStream, true)
    val in = new java.io.BufferedReader(new java.io.InputStreamReader(socket.getInputStream))
    out.println(message)
    val response = in.readLine()
    socket.close()
    response
  }
}
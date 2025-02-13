import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import spray.json._

import scala.concurrent.{ExecutionContextExecutor, Future}
import CocksCipher._

trait JsonSupport extends DefaultJsonProtocol {
  implicit val privateKeyResponseFormat = jsonFormat1(PrivateKeyResponse)
  implicit val publicKeyResponseFormat = jsonFormat1(PublicKeyResponse)
  implicit val publicEResponseFormat = jsonFormat1(PublicEResponse)
}

case class PrivateKeyResponse(privateKey: String)
case class PublicKeyResponse(publicKey: String)
case class PublicEResponse(e: String)

object CocksPKGService extends JsonSupport {
  implicit val system: ActorSystem = ActorSystem("CocksPKGSystem")
  implicit val materializer: Materializer = Materializer(system)
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher


 private val (n, p, q) = setup(512)
  println(s"PKG запущен. Public Key (N) = $n")


  private val privateKeyRoute =
    path("getPrivateKey" / Segment) { id =>
      get {
        val privateKey = extractR(id, n, p, q)
        complete(HttpEntity(ContentTypes.`application/json`, PrivateKeyResponse(privateKey.toString()).toJson.prettyPrint))
      }
    }

  private val publicKeyRoute =
    path("getPublicKey") {
      get {
        complete(HttpEntity(ContentTypes.`application/json`, PublicKeyResponse(n.toString()).toJson.prettyPrint))
      }
    }

  private val routes = privateKeyRoute ~ publicKeyRoute

  def main(args: Array[String]): Unit = {
    Http().newServerAt("0.0.0.0", 8080).bind(routes)
    println("PKG Service запущен на http://localhost:8080")
  }
}

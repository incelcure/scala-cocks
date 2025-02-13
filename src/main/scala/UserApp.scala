import scala.swing._
import scala.swing.event._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer
import javax.swing.{DefaultComboBoxModel, Timer}
import sttp.client3._
import io.circe.parser._
import scala.io.StdIn

import CocksCipher._

class UserApp(name: String, port: String) extends MainFrame {
  private val backend = HttpURLConnectionBackend()

  private val pkgServiceUrl = "http://127.0.0.1:8080"

  private def getPrivateKey(userId: String): Option[BigInt] = {
    val request = basicRequest.get(uri"$pkgServiceUrl/getPrivateKey/$userId")
    val response = request.send(backend)

    response.body match {
      case Right(jsonStr) =>
        parse(jsonStr).flatMap { json =>
          json.hcursor.downField("privateKey").as[BigInt]
        } match {
          case Right(sk) =>
            println(s"sk: $sk")
            Some(sk)
          case Left(_) =>
            println("Ошибка: не удалось распарсить приватный ключ")
            None
        }
      case Left(error) =>
        println(s"Ошибка запроса: $error")
        None
    }
  }

  private def getPublicKey: Option[BigInt] = {
    val request = basicRequest.get(uri"$pkgServiceUrl/getPublicKey")
    val response = request.send(backend)

    response.body match {
      case Right(jsonStr) =>
        parse(jsonStr).flatMap { json =>
          json.hcursor.downField("publicKey").as[BigInt]
        } match {
          case Right(pk) =>
            println(s"pk: $pk")
            Some(pk)
          case Left(_) =>
            println("Ошибка: не удалось распарсить публичный ключ")
            None
        }
      case Left(error) =>
        println(s"Ошибка запроса: $error")
        None
    }
  }

  private val privateKey: BigInt = getPrivateKey(name).get
  private val publicKey: BigInt = getPublicKey.get

  private var contacts: Map[String, String] = Map.empty // name -> (host, publicKey)

  // GUI Components
  private val logArea = new TextArea(10, 40) { editable = false }
  private val messageField = new TextField(25)
  private val recipientItems = new ListBuffer[String]()
  private val recipientField = new TextField(25)
  private val recipientCombo = new ComboBox[String](recipientItems)
  private val connectField = new TextField(15)
  private val portField = new TextField(port, 5)


  title = s"Cocks Chat: $name"
  contents = new BoxPanel(Orientation.Vertical) {
    contents += new Label("My Public Key:")
    contents += new TextField(40) { editable = false; text = publicKey.toString() }
    contents += new FlowPanel(new Label("Connect to:"), connectField, new Label("Port:"), portField)
    contents += new Button("Connect") {
      reactions += { case ButtonClicked(_) => connectToPeer() }
    }
    contents += new Label("Recipient:")
    //contents += recipientField
    contents += recipientCombo
    contents += new Label("Message:")
    contents += messageField
    contents += new Button("Send") {
      reactions += { case ButtonClicked(_) => sendMessage() }
    }
    contents += new ScrollPane(logArea)
    border = Swing.EmptyBorder(10)
  }

  def init(): Unit = {
    Network.startServer(portField.text.toInt)(handleMessage)
  }

  private def connectToPeer(): Unit = {
    val host = connectField.text
    val port = portField.text.toInt
    Network.sendMessage(host, port, s"HELLO_IM:$name").onComplete {
      case scala.util.Success(response) =>
        if(response.startsWith("HELLO_IM:")) {
          val parts = response.split(":")
          Swing.onEDT {
            contacts += (parts(1) -> host)
            recipientItems += parts(1)
            recipientCombo.peer.setModel(new DefaultComboBoxModel(recipientItems.toArray))

            logArea.append(s"Connected to ${parts(1)}\n")
          }
        }
      case _ => logArea.append("Connection failed\n")
    }
  }

  private def sendMessage(): Unit = {
    val message = messageField.text
    recipientCombo.selection.item match {
      case contact if contacts.contains(contact) =>
        val host = contacts(contact)
        val encrypted = encryptString(message, contact, publicKey)
        Swing.onEDT(logArea.append(s"Encrypted: ${encrypted}"))
        Network.sendMessage(host, portField.text.toInt, s"MSG:$encrypted").onComplete {
          case scala.util.Success(_) => Swing.onEDT(logArea.append(s"Sent to $contact: $message\n"))
          case _ => Swing.onEDT(logArea.append("Send failed\n"))
        }
      case _ => Dialog.showMessage(messageField, "Select recipient", "Error", Dialog.Message.Error)
    }
    messageField.text = ""
  }

  private def handleMessage(request: String): String = request match {
    case s"HELLO_IM:$contactName" =>
      Swing.onEDT {
        contacts += (contactName -> connectField.text)
        recipientItems += contactName

        recipientCombo.peer.setModel(new DefaultComboBoxModel(recipientItems.toArray))
        logArea.append(s"$contactName connected\n")
      }
      s"HELLO_IM:$name"

    case s"MSG:$cipher" =>
      Swing.onEDT(logArea.append(s"Cipher: $cipher\n"))
      val decrypted = CocksCipher.decryptString(parseCiphertexts(cipher), privateKey, name, publicKey)
      Swing.onEDT(logArea.append(s"Received: $decrypted\n"))
      "ACK"

    case _ => "ERROR"
  }

  size = new Dimension(500, 600)
  visible = true
}
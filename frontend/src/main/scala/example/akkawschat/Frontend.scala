package example.akkawschat
import org.scalajs.dom.raw._

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.crypto.{ CryptoKey, KeyFormat, crypto }
import upickle.default._
import shared.Protocol

import js.Dynamic.{ global => g }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray
import scala.scalajs.js.typedarray.{ ArrayBuffer, Int8Array }
import scala.util.{ Failure, Success }

object Frontend extends js.JSApp {
  val joinButton: HTMLButtonElement = dom.document.getElementById("join").asInstanceOf[HTMLButtonElement]
  val sendButton: HTMLButtonElement = dom.document.getElementById("send").asInstanceOf[HTMLButtonElement]
  val setButton: HTMLButtonElement = dom.document.getElementById("set").asInstanceOf[HTMLButtonElement]
  val generateButton: HTMLButtonElement = dom.document.getElementById("generate").asInstanceOf[HTMLButtonElement]
  var cipherRelated = new CipherRelated()
  var generatedKey: CryptoKey = _

  var encryptionKey: String = "0123456789ABCDEF0123456789ABCDEF"

  def main(): Unit = {
    val nameField = dom.document.getElementById("name").asInstanceOf[HTMLInputElement]
    val keyField = dom.document.getElementById("key").asInstanceOf[HTMLInputElement]

    val keyText: HTMLElement = dom.document.getElementById("actualkey").asInstanceOf[HTMLElement]

    generateButton.onclick = { (event: MouseEvent) =>
      cipherRelated.generateKey().onComplete {
        case Success(value) =>
          generatedKey = value.asInstanceOf[CryptoKey]
          crypto.subtle.exportKey(KeyFormat.raw, generatedKey).toFuture.onComplete {
            case Success(value2) =>
              var tempByteArray = typedarray.int8Array2ByteArray(value2.asInstanceOf[Int8Array])
              var tempString: String = ""
              for (i <- tempByteArray) {
                var inew = i & 0xff
                g.console.log(inew)
                g.console.log(inew.toHexString)
                tempString += "00" + inew.toHexString takeRight 2
              }
              //g.console.log(newerString)
              g.console.log(tempString)
              keyText.innerHTML = tempString

            case Failure(_) => g.console.log("Error exporting key")
          }
        case Failure(_) => g.console.log("None")
      }
      event.preventDefault()
    }

    setButton.onclick = { (event: MouseEvent) =>
      if (!keyField.value.isEmpty) {
        if (isKeyValid(keyField.value)) {
          cipherRelated.importKey(keyField.value).onComplete {
            case Success(value) =>
              generatedKey = value.asInstanceOf[CryptoKey]
              encryptionKey = keyField.value
              keyText.innerHTML = encryptionKey
            case Failure(_) => g.console.log("None")
          }
        }
      }
      event.preventDefault()
    }
    keyField.onkeypress = { (event: KeyboardEvent) =>
      if (event.keyCode == 13) {
        setButton.click()
        event.preventDefault()
      }
    }
    joinButton.onclick = { (event: MouseEvent) =>
      joinChat(nameField.value)
      event.preventDefault()
    }
    nameField.focus()
    nameField.onkeypress = { (event: KeyboardEvent) =>
      if (event.keyCode == 13) {
        joinButton.click()
        event.preventDefault()
      }
    }
  }

  def joinChat(name: String): Unit = {
    joinButton.disabled = true
    val playground = dom.document.getElementById("playground")
    playground.innerHTML = s"Trying to join chat as '$name'..."
    val chat = new WebSocket(getWebsocketUri(dom.document, name))
    chat.onopen = { (event: Event) ⇒
      playground.insertBefore(p("Chat connection was successful!"), playground.firstChild)
      sendButton.disabled = false

      val messageField = dom.document.getElementById("message").asInstanceOf[HTMLInputElement]
      messageField.focus()
      messageField.onkeypress = { (event: KeyboardEvent) ⇒
        if (event.keyCode == 13) {
          sendButton.click()
          event.preventDefault()
        }
      }
      sendButton.onclick = { (event: Event) ⇒
        cipherRelated.encrypt(messageField.value, generatedKey).onComplete {
          case Success(value) =>
            g.console.log(value.asInstanceOf[js.Any])
            var tempByteArray = typedarray.int8Array2ByteArray(value.asInstanceOf[Int8Array])
            var tempString: String = ""
            for (i <- tempByteArray) {
              var inew = i & 0xff
              tempString += "00" + inew.toHexString takeRight 2
            }
            chat.send(tempString)
            g.console.log(tempString)
            messageField.value = ""
            messageField.focus()
            event.preventDefault()
          case Failure(something) =>
            g.console.error(something.asInstanceOf[js.Any])
        }
      }

      event
    }
    chat.onerror = { (event: ErrorEvent) ⇒
      playground.insertBefore(p(s"Failed: code: ${event.colno}"), playground.firstChild)
      joinButton.disabled = false
      sendButton.disabled = true
    }
    chat.onmessage = { (event: MessageEvent) ⇒
      val wsMsg: Protocol.Message = read[Protocol.Message](event.data.toString)

      wsMsg match {
        case Protocol.ChatMessage(sender, message) ⇒

          cipherRelated.decrypt(message, generatedKey).onComplete {
            case Success(value) =>
              var tempByteArray = typedarray.int8Array2ByteArray(value.asInstanceOf[Int8Array])
              var tempString: String = ""
              var tempDecryptedString: String = ""
              for (i <- tempByteArray) {
                var inew = i & 0xff
                tempDecryptedString += inew.toChar
                tempString += "00" + inew.toHexString takeRight 2
              }
              writeToArea(s"$sender said: $message c:( $tempDecryptedString )")
            case Failure(something) =>
              g.console.error(something.asInstanceOf[js.Any])
          }
        case Protocol.Joined(member, _) ⇒ writeToArea(s"$member joined!")
        case Protocol.Left(member, _)   ⇒ writeToArea(s"$member left!")
      }
    }
    chat.onclose = { (event: Event) ⇒
      playground.insertBefore(p("Connection to chat lost. You can try to rejoin manually."), playground.firstChild)
      joinButton.disabled = false
      sendButton.disabled = true
    }

    def writeToArea(text: String): Unit =
      playground.insertBefore(p(text), playground.firstChild)
  }

  def getWebsocketUri(document: Document, nameOfChatParticipant: String): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

    s"$wsProtocol://${dom.document.location.host}/chat?name=$nameOfChatParticipant"
  }

  def p(msg: String): Element = {
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = msg
    paragraph
  }

  def isKeyValid(candKey: String): Boolean = {
    candKey.length match {
      case 32 | 48 | 64 =>
        try {
          for (i <- candKey) {
            Integer.parseInt(i.toString, 16)
          }
          true
        } catch {
          case _: Throwable => false
        }
      case _ => false
    }
  }
}
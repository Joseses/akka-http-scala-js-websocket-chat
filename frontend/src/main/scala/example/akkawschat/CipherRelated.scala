package example.akkawschat

/**
 * Created by josema on 29/04/17.
 */

import org.scalajs.dom.crypto._

import scala.scalajs.js
import js.Dynamic.{ global => g }
import scala.scalajs.js.typedarray.Uint8Array
import scala.concurrent.Future

class CipherRelated {

  var generatedKey: CryptoKey = _
  var plaintextKey: String = ""
  val ivPerm = js.Array(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B)

  def generateKey(): Future[Any] = {
    crypto.subtle.generateKey(
      AesKeyAlgorithm.apply("AES-GCM", 128),
      extractable = true, js.Array[KeyUsage](
        KeyUsage.decrypt, KeyUsage.encrypt
      )
    ).toFuture
  }

  def decrypt(message: String, generatedKey: CryptoKey): Future[Any] = {
    val byteArray: Array[Byte] = new Array[Byte](message.length / 2)
    var x = 2
    for (i <- 0 until message.length / 2) {
      g.console.log(i)
      g.console.log(message.substring(x - 2, x))
      byteArray(i) = Integer.parseInt(message.substring(x - 2, x), 16).toByte
      x += 2
    }
    val arrayBuffer = new BigInteger(js.typedarray.byteArray2Int8Array(byteArray))
    val initV = new Uint8Array(ivPerm)
    crypto.subtle.decrypt(
      AesGcmParams.apply("AES-GCM", initV, new Uint8Array(0), 128),
      generatedKey,
      arrayBuffer
    ).toFuture
  }

  def encrypt(message: String, cryptoKey: CryptoKey): Future[Any] = {
    val initV = new Uint8Array(ivPerm)
    val arrayBuffer = new BigInteger(js.typedarray.byteArray2Int8Array(message.getBytes("UTF-8")))

    crypto.subtle.encrypt(
      AesGcmParams.apply("AES-GCM", initV, new Uint8Array(0), 128),
      cryptoKey,
      arrayBuffer
    ).toFuture
  }

  def importKey(keyToBeImported: String): Future[Any] = {
    //val stringArray: Array[String] = keyToBeImported.split("(?<=\\G..)")
    var byteArray: Array[Byte] = new Array[Byte](keyToBeImported.length / 2)
    //for (i <- 0 to stringArray.length) {
    //  byteArray(i) = Integer.parseInt(stringArray(i), 16).toByte
    //}
    var x = 2
    for (i <- 0 until keyToBeImported.length / 2) {
      g.console.log(i)
      g.console.log(keyToBeImported.substring(x - 2, x))
      byteArray(i) = Integer.parseInt(keyToBeImported.substring(x - 2, x), 16).toByte
      x += 2
    }
    val arrayBuffer = new BigInteger(js.typedarray.byteArray2Int8Array(byteArray))

    crypto.subtle.importKey(
      KeyFormat.raw,
      arrayBuffer,
      "AES-GCM",
      extractable = true,
      js.Array[KeyUsage](
        KeyUsage.decrypt, KeyUsage.encrypt
      )
    ).toFuture
  }
}

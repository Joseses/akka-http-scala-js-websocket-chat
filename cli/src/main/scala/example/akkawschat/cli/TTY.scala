package example.akkawschat.cli

object TTY {
  val ANSI_ESCAPE = "\u001b["
  val SAVE: String = ANSI_ESCAPE + "s"
  val RESTORE: String = ANSI_ESCAPE + "u"
  val ERASE_LINE: String = ANSI_ESCAPE + "K"
  val GRAY: String = ANSI_ESCAPE + "0;1m"

  def cursorLeft(chars: Int): String =
    s"$ANSI_ESCAPE${chars}D"

  import sys.process._

  // touch this to ensure that the TTY isn't left in a broken state
  lazy val ensureShutdownHook: Unit =
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        saneStty()
      }
    })

  // stty arguments shamelessly stolen from ammonite (https://github.com/lihaoyi/Ammonite/blob/master/terminal/src/main/scala/ammonite/terminal/Utils.scala#L71)
  def noEchoStty(): String = {
    ensureShutdownHook
    "stty -F /dev/tty -echo -icanon min 1 -icrnl -inlcr".!!
  }
  def saneStty(): String = "stty -F /dev/tty sane".!!
}
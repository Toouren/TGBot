import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.io.Source


object DebriefingBot extends TelegramBot with Polling {

  lazy val token: String = Source.fromFile("src\\bot.token").getLines().mkString
  private val bot = new Bot

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text) {
      request(SendMessage(msg.source, bot.doCommand(text.concat("\n"))))
      SendMessage()
      print(text)
    }
  }
}
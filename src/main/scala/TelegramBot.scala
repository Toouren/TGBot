import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import scala.io.Source

import scala.util.{Failure, Success}


object TelegramBot extends TelegramBot with Polling {

  lazy val token: String = Source.fromFile("src\\bot.token").getLines().mkString
  private val executor = Executor

  val parse = (command: String) => CommandParser.parse(command)

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text) {
      request(SendMessage(msg.source,
        parse(text) match {
          case Success(command) =>
            executor.executeCommand(msg.source, command)
          case Failure(e) =>
            "Syntax error. Bad command"
      }))
    }
  }
}
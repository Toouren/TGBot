class Bot {
  private val parser = CommandParser
  private val executor = CommandExecutor

  def doCommand(string: String): String = {
    executor.applyCommand(parser.parse(string))
  }
}

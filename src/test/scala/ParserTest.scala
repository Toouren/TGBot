import org.scalatest.{FlatSpec, Matchers, MustMatchers}
import scala.util.Random
import scala.util.matching.Regex

class ParserTest extends FlatSpec with MustMatchers{

  private val kindsOfList = Map("no_sets" -> "Anonymity:  true\nVisibility:  false\nStart Time:  Start time not set\nEnd Time:  End time not set",
    "set_visibility" -> "Anonymity:  true\nVisibility:  true\nStart Time:  Start time not set\nEnd Time:  End time not set",
    "set_anonymity" -> "Anonymity:  false\nVisibility:  false\nStart Time:  Start time not set\nEnd Time:  End time not set",
    "set_visibility_anonymity" -> "Anonymity:  false\nVisibility:  true\nStart Time:  Start time not set\nEnd Time:  End time not set")

  private val randomPhrases = Set("Have you got any pets?", "I don't have any children", "Are you religious?", "Do you believe in life after death?")

  private val createPollPattern = """Poll with \((\d*)\) id was created"""

  private val bot = new Bot

  "There are 10 same polls" should "be created" in {
    for (i <- 1 to 10) {
      assertResult(s"Poll with ($i) id was created"){
        bot.doCommand(s"/create_poll (go $i party ((on Monday))?) (yes)")
      }
    }
  }

  "There are 10 same polls" should "be deleted" in {
    for (i <- 1 to 10) {
      assertResult(s"Poll with ($i) was deleted"){
        bot.doCommand(s"/delete_poll ($i)")
      }
    }
  }

  "There is list with no sets" should "be compared" in {
    val testBot = new Bot
    testBot.doCommand(s"/create_poll (Go on scala party ((on Monday))?)") must fullyMatch regex createPollPattern
    testBot.doCommand("/list") must include(kindsOfList("no_sets"))
  }

  "There is list with visibility set" should "be compared" in {
    val testBot = new Bot
    testBot.doCommand(s"/create_poll (Go on scala party ((on Tuesday))?) (yes)(continuous)") must fullyMatch regex createPollPattern
    testBot.doCommand("/list") must include(kindsOfList("set_visibility"))
  }

  "There is list with anonymity set" should "be compared" in {
    val testBot = new Bot
    testBot.doCommand(s"/create_poll (Go on scala party ((on Wednesday))?) (no)") must fullyMatch regex createPollPattern
    testBot.doCommand("/list") must include(kindsOfList("set_anonymity"))
  }

  "There is list with visibility and anonymity sets" should "be compared" in {
    val testBot = new Bot
    bot.doCommand(s"/create_poll (Go on scala party ((on Thursday))?) (no)(continuous)") must fullyMatch regex createPollPattern
    testBot.doCommand("/list") must include(kindsOfList("set_anonymity"))
  }

  "There is trying to delete uncreated polls" should "be exception" in {
    for(i <- 1 to 10) {
      assertResult(s"Poll with ($i) id not found") {
        bot.doCommand(s"/delete_poll ($i)")
      }
    }
  }

  "There is trying to start uncreated polls" should "be exception" in {
    for(i <- 1 to 10) {
      assertResult(s"Poll with ($i) id not found") {
        bot.doCommand(s"/start_poll ($i)")
      }
    }
  }

  "There is trying to start created polls" should "be exception" in {
    val testBot = new Bot
    val pattern = createPollPattern.r
    for(i <- 1 to 5) {
      val pollId = (pattern findAllIn testBot.doCommand("/create_poll (love scala?) (yes)")).group(1)
      assertResult(s"Poll with ($pollId) id was started") {
        testBot.doCommand(s"/start_poll ($pollId)")
      }
    }
  }

  "There is trying to stop uncreated polls" should "be exception" in {
    for(i <- 1 to 10) {
      assertResult(s"Poll with ($i) id not found") {
        bot.doCommand(s"/stop_poll ($i)")
      }
    }
  }

  "There is trying to stop created polls" should "be exception" in {
    val testBot = new Bot
    val pattern = createPollPattern.r
    for(i <- 1 to 5) {
      val pollId = (pattern findAllIn testBot.doCommand("/create_poll (love scala?) (yes)")).group(1)
      assertResult(s"Poll with ($pollId) id was stop") {
        testBot.doCommand(s"/stop_poll ($pollId)")
      }
    }
  }

  "There is trying to get result from uncreated polls" should "be exception" in {
    for(i <- 1 to 10) {
      assertResult(s"Poll with ($i) id not found") {
        bot.doCommand(s"/result ($i)")
      }
    }
  }

  "There is trying to check any bad commands" should "be message about bat command" in {
    val rnd = new Random
    val request = randomPhrases.toVector(rnd.nextInt(randomPhrases.size))
    assertResult("Bed command: \n".concat(request)) {
      bot.doCommand(request)
    }
  }
}

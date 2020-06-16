import org.scalatest.funsuite.AnyFunSuite
import games._
import cards._

class MainTest extends AnyFunSuite {
  test("valueOfCard function test") {
    val blackjack = Blackjack(1)
    assert(blackjack.valueOfCard(Card(Hearts, Numerical(2)), 10) == 2)
    assert(blackjack.valueOfCard(Card(Hearts, Ace), 5) == 11)
    assert(blackjack.valueOfCard(Card(Hearts, Ace), 13) == 1)
    assert(blackjack.valueOfCard(Card(Hearts, Queen), 2) == 10)
  }

  test("pointsOfCard function test") {
    val blackjack = Blackjack(1)
    assert(blackjack.pointsOfCards(3, List(Card(Hearts, Ace), Card(Hearts, Ace), Card(Hearts, Ace))) == 13)
    assert(blackjack.pointsOfCards(2, List(Card(Hearts, Ace), Card(Hearts, King))) == 21)
    assert(blackjack.pointsOfCards(3, List(Card(Hearts, Ace), Card(Hearts, Numerical(2)), Card(Hearts, Numerical(3)))) == 16)
  }
}

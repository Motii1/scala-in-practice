import org.scalatest.funsuite.AnyFunSuite
import cards._
import deck._

class MainTest extends AnyFunSuite {
  test("toString should work correct") {
    val exampleCard = Card(Hearts, Queen)
    assert(exampleCard.rank.toString == "Queen")
    assert(exampleCard.color.toString == "Hearts")

    val exampleCard2 = Card(Spades, Numerical(2))
    assert(exampleCard2.color.toString == "Spades")
    assert(exampleCard2.rank.toString == "Numerical(2)")
  }

  test("Default deck should be a standard deck") {
    val deck = Deck()
    assert(deck.isStandard)
  }

  test("duplicatesOfCard function test") {
    val deck = Deck()
    assert(deck.duplicatesOfCard(Card(Hearts, Ace)) == 1)
  }

  test("amountOfColor function test") {
    val deck = Deck()
    assert(deck.amountOfColor(Clubs) == 13)
  }

  test("amountOfNumerical function test") {
    val deck = Deck()
    assert(deck.amountOfNumerical(Numerical(2)) == 4)
  }

  test("amountWithNumerical function test") {
    val deck = Deck()
    assert(deck.amountWithNumerical == 36)
  }

  test("amountWithFace function test") {
    val deck = Deck()
    assert(deck.amountWithFace == 12)
  }

  test("amountOfFace function test") {
    val deck = Deck()
    assert(deck.amountOfFace(Jack) == 4)
  }
}
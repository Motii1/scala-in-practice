import cards._
import deck._
import games._

object Main extends App {
    val exampleCard = Card(Hearts, Queen)
    require(exampleCard.rank.toString == "Queen")
    require(exampleCard.color.toString == "Hearts")

    val exampleCard2 = Card(Spades, Numerical(2))
    require(exampleCard2.color.toString == "Spades")
    require(exampleCard2.rank.toString == "Numerical(2)")

    val deck = Deck()
    require(deck.isStandard)
    require(deck.duplicatesOfCard(Card(Hearts, Ace)) == 1)
    require(deck.amountOfColor(Clubs) == 13)
    require(deck.amountOfNumerical(Numerical(2)) == 4)
    require(deck.amountWithNumerical == 36)
    require(deck.amountWithFace == 12)
    require(deck.amountOfFace(Jack) == 4)

    val blackjack = Blackjack(1)
    require(blackjack.valueOfCard(Card(Hearts, Numerical(2)), 10) == 2)
    require(blackjack.valueOfCard(Card(Hearts, Ace), 5) == 11)
    require(blackjack.valueOfCard(Card(Hearts, Ace), 13) == 1)
    require(blackjack.valueOfCard(Card(Hearts, Queen), 2) == 10)

    require(blackjack.pointsOfCards(3, List(Card(Hearts, Ace), Card(Hearts, Ace), Card(Hearts, Ace))) == 13)
    require(blackjack.pointsOfCards(2, List(Card(Hearts, Ace), Card(Hearts, King))) == 21)
    require(blackjack.pointsOfCards(3, List(Card(Hearts, Ace), Card(Hearts, Numerical(2)), Card(Hearts, Numerical(3)))) == 16)

    blackjack.first21
    
    blackjack.play(6)
}

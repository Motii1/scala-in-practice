package deck

import cards._
import scala.util.Random.shuffle

class Deck(val cards: List[Card]) {
  def pull() = Deck(cards.tail)

  def push(c: Card) = Deck(c :: cards)

  def push(color: Color, value: Rank): Deck = push(Card(color, value))

  def duplicatesOfCard(card: Card): Int = cards.length - cards.filter(c => c != card).length

  def amountOfColor(color: Color) = cards.foldLeft(0)((n: Int, c: Card) => if (c.color == color) n + 1 else n)

  def amountOfNumerical(numerical: Numerical): Int = cards.filter(c => c.rank == numerical).length

  val amountWithNumerical: Int = cards.filter(c => c.rank.isInstanceOf[Numerical]).length

  def amountOfFace(face: Face) = cards.filter(c => c.rank == face).length

  val amountWithFace: Int = cards.filter(c => c.rank.isInstanceOf[Face]).length

  val isStandard: Boolean = {
    (for { c <- Deck.standardDeckCards
          if cards contains c
    } yield c).length == Deck.standardDeckSize
  }
}

object Deck {
  val standardDeckSize = 52

  val standardDeckCards = for {
    color <- List(Spades, Hearts, Diamonds, Clubs)
    rank <- List(Ace, Jack, Queen, King) ::: (for (i <- (2 to 10).toList) yield Numerical(i))
  } yield Card(color, rank)

  def apply() = new Deck(shuffle(standardDeckCards))

  def apply(cards: List[Card]) = new Deck(cards)
}

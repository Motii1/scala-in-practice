package games

import deck._
import cards._
import scala.util.Random.shuffle
import scala.collection.immutable.HashMap

class Blackjack(deck: Deck) {
  def valueOfCard(c: Card, currentSum: Int) = c.rank match {
    case n: Numerical => n.num
    case f: Face => 10
    case a: Ace => if (currentSum + 11 > 21) 1 else 11 // always taking the best option for the player
  }

  def pointsOfCards(n: Int, cards: List[Card]) = {
    require(n >= 0)
    def helper(n: Int, points: Int, c: List[Card]): Int = n match {
      case 0 => points
      case num => helper(num - 1, points + valueOfCard(c.head, points), c.tail)
    }

    helper(n, 0, cards)
  }

  def createPrettyCard(c: Card): Array[String] = {
    val colorSymbols = HashMap(Spades -> '♠', Diamonds -> '♦', Hearts -> '♥', Clubs -> '♣')
    val notNumericalValues = HashMap(Jack -> 'J', Queen -> 'Q', King -> 'K', Ace -> 'A')
    val symbol = c.rank match {
      case n: Numerical => n.num.toString
      case x: Rank => notNumericalValues(x)
    }
    val spaceOrNot = if (symbol == "10") "" else " "

    val lines: Array[String] = new Array(9)
    lines(0) = "┌─────────┐"
    lines(1) = s"│${symbol}${spaceOrNot}       │"
    lines(2) = "│         │"
    lines(3) = "│         │"
    lines(4) = s"│    ${colorSymbols(c.color)}    │"
    lines(5) = "│         │"
    lines(6) = "│         │"
    lines(7) = s"│       ${spaceOrNot}${symbol}│"
    lines(8) = "└─────────┘"

    lines
  }

  def prettyPrintCards(arr: Array[Array[String]]) = {
    for (i <- 0 to 8) {
      for (j <- 0 until arr.length) {
        print(arr(j)(i))
      }
      println()
    }
  }

  def play(n: Int): Unit = {
    val arr = new Array[Array[String]](n)
    for (i <- 0 until n) {
      arr(i) = createPrettyCard(deck.cards(i))
    }
    prettyPrintCards(arr)
    println(s"\nSum of points of above cards: ${pointsOfCards(n, deck.cards)}")
  }

  lazy val all21: List[List[Card]] = for {
    i <- (0 until deck.cards.length).toList
    j <- i + 1 until deck.cards.length
    if pointsOfCards(j - i, deck.cards.slice(i, j)) == 21
  } yield deck.cards.slice(i, j)

  def first21(): Unit = {
    val first21Subsequence = all21.head
    val cards = new Array[Array[String]](first21Subsequence.length)

    for (i <- 0 until first21Subsequence.length) {
      cards(i) = createPrettyCard(first21Subsequence(i))
    }

    prettyPrintCards(cards)
  }
}

object Blackjack {
  def apply(numOfDecks: Int) = {
    require(numOfDecks > 0)

    def composeDecks(n: Int): List[Card] = {
      if (n == 1) {
        Deck.standardDeckCards
      } else {
        Deck.standardDeckCards ::: composeDecks(n - 1)
      }
    }

    val startDeck = Deck(shuffle(composeDecks(numOfDecks)))
    new Blackjack(startDeck)
  }
}
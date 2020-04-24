package cards

sealed abstract class Color extends Product with Serializable
case object Diamonds extends Color
case object Spades extends Color
case object Clubs extends Color
case object Hearts extends Color

sealed abstract class Rank extends Product with Serializable

sealed abstract class Face extends Rank
case object Jack extends Face
case object Queen extends Face
case object King extends Face

sealed abstract class Ace extends Rank
case object Ace extends Ace

case class Numerical(val num: Int) extends Rank {
  require(num >= 2 && num <= 10)
}

case class Card(val color: Color, val rank: Rank)
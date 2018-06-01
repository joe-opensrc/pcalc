package pcalc
import pcalc.types._
import enumeratum._

package object types {
  type Cards = Seq[Card]
}

sealed abstract class CardProperty( val c: Char, val v: Int ) extends EnumEntry with Ordered[CardProperty] {

  def toChar = c 
  def value = v

  def compare( that: CardProperty ): Int = { this.v - that.v }
  def  equals( that: CardProperty ): Boolean = { this.equals(that) } 

}


sealed abstract class Rank( override val c: Char, override val v: Int ) extends CardProperty( c,v ) {}
sealed abstract class Suit( override val c: Char, override val v: Int ) extends CardProperty( c,v ) {}


case object Rank extends Enum[Rank] {

  val values = findValues

  //23456789TJQKA
  case object Two   extends Rank( '2',  2 )
  case object Three extends Rank( '3',  3 )
  case object Four  extends Rank( '4',  4 )
  case object Five  extends Rank( '5',  5 )
  case object Six   extends Rank( '6',  6 )
  case object Seven extends Rank( '7',  7 )
  case object Eight extends Rank( '8',  8 )
  case object Nine  extends Rank( '9',  9 )
  case object Ten   extends Rank( 'T', 10 )
  case object Jack  extends Rank( 'J', 11 )
  case object Queen extends Rank( 'Q', 12 )
  case object King  extends Rank( 'K', 13 )
  case object Ace   extends Rank( 'A', 14 )

}

case object Suit extends Enum[Suit] {

  val values = findValues
  
  //♠♣♥♦
  case object Heart   extends Suit( '♥', 100 )
  case object Club    extends Suit( '♣', 100 )
  case object Diamond extends Suit( '♦', 100 )
  case object Spades  extends Suit( '♠', 100 )

}

class Card( private var _rank: Rank, private var _suit: Suit ) extends Ordered[Card]{

  def rank = _rank
  def suit = _suit

  def rank_= ( r: Rank ) { this.rank = r }
  def suit_= ( s: Suit ) { this.suit = s }

  override def compare( that: Card ): Int = {
    this.rank.value - that.rank.value
  }

  override def toString(): String = {
    return rank.toChar + "" + suit.toChar
  }

}

class Hand( private var _cs: Cards ) {

  def cards = _cs

  def merge( h: Hand ): Unit = {
    this.cards ++ h.cards
  }

  def sort: Cards = { this.cards.sorted }
  def sorted: Cards = { this.sort }
  override def toString(): String = {
    "[" + this.cards.mkString("|") + "]" 
  }

}

object Main {

  import Rank._
  import Suit._

  def main( args: Array[String] ): Unit = {

    val deck = for { s <- Suit.values; r <- Rank.values  } yield { new Card(r,s) }

    val c1 = new Card( Four, Heart )
    val c2 = new Card( Three, Heart )
    val h1 = new Hand( List( c1, c2 ) )

    println(deck) 

  }
}

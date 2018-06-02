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
  def  equals( that: CardProperty ): Boolean = { this.v == that.v  } 

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

object Card {

  /** aka abusing Option.get **/
  def charToRank( c: Char): Rank = {
    Rank.values.find( r => r.c == c ).get
  }

  def charToSuit( c: Char): Suit = {
    Suit.values.find( s => s.c == c ).get
  }

  def apply( cstr: String ): Card = {

    val rchar = cstr(0)
    val schar = cstr(1)
    
    new Card ( charToRank( rchar ), charToSuit( schar ) )

  }

}

class Card( private var _rank: Rank, private var _suit: Suit ) extends Ordered[Card] {

  def rank = _rank
  def suit = _suit

  def rank_= ( r: Rank ) { this.rank = r }
  def suit_= ( s: Suit ) { this.suit = s }

  override def compare( that: Card ): Int = {
    this.rank.value - that.rank.value
  }

  override def equals( that: Any ): Boolean = that match {
    case that: Card => that.isInstanceOf[Card] && this.rank == that.rank && this.suit == that.suit 
    case _ => false
  }

  override def toString(): String = {
    return rank.toChar + "" + suit.toChar
  }

}

class Hand( private var _cs: Cards ) extends Ordered[Hand] {

  def cards = _cs
  def cards_= ( cs: Cards ): Cards = { this.cards = cs }

  def merge( h: Hand ): Unit = {
    this.cards ++ h.cards
  }

  def sort: Cards = { this.cards.sorted }
  def sorted: Cards = { this.sort }

  override def equals( that: Any ): Boolean = { 
    that match {
      case that: Hand => this.cards == that.cards
      case _ => false
    } 
  } 

  override def compare( that: Hand ) = {
    /** hand comparison to defined later :O **/
    0
  }

  override def toString(): String = {
    "[" + this.cards.mkString("|") + "]" 
  }

}

object Dealer {
  def newDeck: Cards = { 
    for { s <- Suit.values; r <- Rank.values  } yield { new Card(r,s) } 
  }

  def shuffle( d: Cards ): Cards = { 
    scala.util.Random.shuffle( d ) 
  }

  def deal( d: Cards, n: Int ): Hand = { 
    this.makeHand( d.take(n) ) 
  }

  def makeHand( cs: Cards ): Hand = { 
    new Hand( cs ) 
  }

  /** TODO: remove warning about type erasure (if poss) **/
  def ensureRemoved( d: Cards, a: Any ): Cards = {
    var r: Cards = d
    val cs = { 
      a match {
        case a: Hand  => a.cards
        case a: Cards => a
        case _        => List()
      }
    }
    
    for ( c <- cs ) { r = r.filterNot( _.equals(c) ) }
    return r 

  } 

  def apply( d: Cards ): Dealer = { 
    new Dealer(d) 
  }

}

class Dealer( val _d: Cards = Dealer.newDeck ) {
  var deck = _d
  
  def newDeck() = { this.deck = Dealer.newDeck }
  def shuffleDeck() = { this.deck = Dealer.shuffle( this.deck ) }
  def deal( n: Int ): Hand = { val h = Dealer.deal( this.deck, n ); this.deck = this.deck.drop(n); h }
  def ensureRemoved( a: Any ) = { this.deck = Dealer.ensureRemoved( this.deck, a ) }
}

object Main {

  import Rank._
  import Suit._

  def main( args: Array[String] ): Unit = {
println( new Card( Three, Club ) == new Card( Two, Club ) )
//    val deck = Dealer.shuffle( Dealer.newDeck )
    val dealer = new Dealer()

    val c1 = new Card( Two, Heart )
    val c2 = new Card( Three, Heart )

    val h1 = new Hand( List(c1,c2) )

//    println( Card.valueToRank( A ) 

    //dealer.ensureRemoved( h1 )
    

    val h2 = dealer.deal( 2 ) 

    println( h1 + "--" + h2 )
    println( h1 == h2 )
    println( dealer.deck ) 

  }
}

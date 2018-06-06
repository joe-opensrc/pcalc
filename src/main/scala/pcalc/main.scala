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
  case object Two    extends Rank( '2',    1 )
  case object Three  extends Rank( '3',    2 )
  case object Four   extends Rank( '4',    4 )
  case object Five   extends Rank( '5',    8 )
  case object Six    extends Rank( '6',   16 )
  case object Seven  extends Rank( '7',   32 )
  case object Eight  extends Rank( '8',   64 )
  case object Nine   extends Rank( '9',  128 )
  case object Ten    extends Rank( 'T',  256 )
  case object Jack   extends Rank( 'J', 1024 )
  case object Queen  extends Rank( 'Q', 2048 )
  case object King   extends Rank( 'K', 4096 )
  case object Ace    extends Rank( 'A', 8192 )

}

case object Suit extends Enum[Suit] {

  val values = findValues
  
  //♠♣♥♦
  case object Heart   extends Suit( '♥', 16384 )
  case object Club    extends Suit( '♣', 32768 )
  case object Diamond extends Suit( '♦', 65536 )
  case object Spade   extends Suit( '♠', 131072 )

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
    case that: Card => that.isInstanceOf[Card] && this.hashCode == that.hashCode 
    case _ => false
  }

  override def hashCode(): Int = {
    this.rank.value + this.suit.value 
  }

  override def toString(): String = {
    return rank.toChar + "" + suit.toChar
  }

}

object Math {

  def factFold(n: BigInt): BigInt = { 
    val one: BigInt = 1
    (one to n).foldLeft(one)( (a,b) => (a*b) )
  }
 
  def pnk( n: BigInt, k: BigInt ): BigInt = {
    if ( k > n ) { return 0 } else {
    return factFold(n) / factFold(n-k) 
    }
  }

  def nck( n: BigInt, k: BigInt ): BigInt = {
   if ( k > n ) { return 0 } else {
    return pnk(n, k) / factFold(k) 
   }
  }
 
}

class Hand( private var _cards: Cards  ) { //extends Ordered[Hand] {

  def cards = _cards

  def merge( h: Hand ): Unit = {
    this._cards = this.cards ++ h.cards
  }

  def sort(): Unit = { this._cards = this._cards.sorted }
  def sorted: Hand = { this.sort(); this }

  override def equals( that: Any ): Boolean = { 
    that match {
      case that: Hand => this.cards.sorted.map( _.rank.value ).sum == that.cards.sorted.map( _.rank.value ).sum
      case _ => false
    } 
  } 

  override def hashCode: Int = {
    val hc = this.cards.map( x => x.hashCode ).sum
//    println( "hc: " + hc + " -> " + this.cards ) 
    hc
  }

  override def toString(): String = {
    "[" + this._cards.mkString("|") + "]" 
  }

}

object Hand {

  def merge( h1: Hand, h2: Hand ): Hand = {
    val cs = h1.cards ++ h2.cards
    new Hand( cs )
  }


  def apply( s: String ): Hand = {
    
    val hstr = s.split('|')
    val css = for ( cstr <- hstr) yield {  Card(cstr)  }  

    new Hand( css )
 
  }

  def rank( h: Hand ) = {
//    val cs = h.cards.sorted
    val cs = Hand("2♣|3♣|4♣|5♣|6♣|Q♣|A♣").cards
    println(cs)
    val ranks = { cs.map( _.rank ).groupBy(identity)  }
    val suits = { cs.map( _.suit ).groupBy(identity).mapValues(_.size) }
    val flushed = suits.filter(_._2 >= 5).nonEmpty 
    val (threes, twos) = ranks.mapValues(_.size).filter(_._2 >= 2).partition( _._2 > 2 ) //.toList.sortWith( _._1 > _._1)

    var lastcard = cs.last
    val str8_prep = { if ( lastcard.rank == Rank.Ace ){ lastcard +: cs } else { cs } }
//    val wheel_str8 = css.map( _.hashCode ).sum 

      /** reverse gives left to right scan; means we find highest straight first (more often than not) **/
      val str8_hands = str8_prep.sliding(5).toList.reverse
      val str8_check = str8_hands.map( 
                           _.sliding(2).map{ 
                              case Seq(x,y) => Rank.valuesToIndex( y.rank ) - Rank.valuesToIndex( x.rank ) }.toList ).map{ 
                                case List(-12,1,1,1) | List(1,1,1,1) => true
                                case _ => false 
                        } 

      /** not strictly idiomatic, but readable **/
      val lc = str8_check.indexOf( true )
      val (hrank, str8_val) = str8_hands.lift(lc) match {
        case Some(x) => 
          /** if all 5 cards are the same then the groupedBy size is unity **/
          val flsh = x.map( _.suit ).groupBy(identity).mapValues(_.size).size == 1
          val hr = flsh match { 
            case true => "Hand.Rank.StraightFlush"
            case false => "Hand.Rank.Straight"
          }
          (Some(hr), x.last)

        case _ => (None, None)

      }


//     println(cs)
//     println(ranks)
//     println(suits)
//     println(flushed)    
//     println( twos.toList.sortWith( _._1 > _._1 ) )
//     println( threes.toList.sortWith( _._1 > _._1 ) )
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

    val dealer = new Dealer()
    dealer.shuffleDeck()

    val c1 = new Card( King, Heart )
    val c2 = new Card( Ten, Heart )

    val h1 = new Hand( List(c1,c2) )

    dealer.ensureRemoved( h1 )

    val h2 = dealer.deal( 7 ) 
    h2.sort()

    Hand.rank( h2 )
//    println (  Hand.merge( h1, h2 ).sorted )

//    println( h1.sorted + "--" + h2 )
//    println( h1 == h2 )
//    println( dealer.deck.size ) 

  }
}

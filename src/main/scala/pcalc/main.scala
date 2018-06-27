package pcalc
import pcalc.types._
import enumeratum._

package object types {
  type Cards = Seq[Card]
}

sealed abstract class CardProperty( val c: Char, val v: Int ) extends EnumEntry with Ordered[CardProperty] {

  def toChar = c 
  def value = v

  override def compare( that: CardProperty ): Int = { this.v - that.v }
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

  val HANDSIZE = 5

  def merge( h1: Hand, h2: Hand ): Hand = {
    val cs = h1.cards ++ h2.cards
    new Hand( cs )
  }


  def apply( s: String ): Hand = {
    
    val hstr = s.split('|')
    val css = for ( cstr <- hstr) yield {  Card(cstr)  }  

    new Hand( css )
 
  }

  /** check for straights: 
        return Hand.Rank.RoyalFlush, 
        return Hand.Rank.StraightFlush, 

        Hand.Rank.Straight 

        if Hand.Rank.Straight or None
          check for: 
            return Hand.Rank.FourOfAKind
            return Hand.Rank.FullHouse
            Hand.Rank.TwoPair
            Hand.Rank.Pair

            check for:
              return Hand.Rank.Flush
  
            if None:  
              return Hand.Rank.{TwoPair,Pair,HighCard}
         
        

  */

  def cardsAreSequential( cs: Cards ) = {
  

println("cs: " + cs.sorted.reverse )

    cs.sorted.reverse.sliding(2).map{ 
      case Seq(x,y) => Rank.valuesToIndex( x.rank ) - Rank.valuesToIndex( y.rank )
    }.toList match {
      case List(-12,1,1,1) | List(1,1,1,1) => true
      case _ => false
    }

  }

  def areFlushed( cs: Cards ): Boolean = {
    cs.tail.count( _.suit == cs.head.suit ) == cs.length - 1
  }

  def genStr8s( cs: Cards ): List[Cards] = {
    
    val rankMap = cs.sorted.reverse.groupBy( _.rank )

    // ranksOne -- cards for which there is only possibility
    val ranksOne = rankMap.filter( _._2.size == 1 ).values.flatten.toList
    // ranksMany -- cards which have duplicate rank
    val ranksMany = rankMap.filter( _._2.size > 1 )

    // ranksManyCount -- how many Duplicate card groups?
    val ranksManyCount = ranksMany.keys.size

    // one set of two, two sets of two, or one set of three, but no more
    // nope! FourOfAKind == 2-Twos => canBeStr8! doh!
    val totalDupedCards = ranksMany.values.flatten.size 

    val canBeStr8 = (ranksManyCount, totalDupedCards) match {
      case (1,2) | (2,4) | (1,3) => true
      case _ => false
    }

    println("rankMap: " + rankMap)
    println("ranksOne: " + ranksOne)
    // wired duplicates sneeking back in
    println("ranksMany: " + ranksMany ) //.values.flatten.map( _ :: ranksOne ).map( _.sorted ).map( _.sliding(5).toList ).flatten.collect{ case x if cardsAreSequential(x) => x }.toList ) 
    println("ranksManyCount: " + ranksManyCount ) 
    println("canBeStr8: " + canBeStr8 )

    val str8s = canBeStr8 match {

      case true =>

        if( ranksManyCount == 1 ){
          ranksMany.values.flatten.map( _ :: ranksOne ).map( _.sorted ).map( _.sliding(5).toList ).flatten.collect{ case x if cardsAreSequential(x) => x }.toList 
        }else{
          if( ranksManyCount == 2 ){
            ranksMany.values.flatten.toList.combinations(2).filter{ 
                  case List(x,y) if x.rank != y.rank => true; 
                  case _                             => false }.map( _.union(ranksOne).sorted ).collect{ case x if cardsAreSequential(x) => x }.toList
          }else{ 
            List()
          }
        }
      

      case false =>
        List()

     }


  return str8s

} 

                 //♠ ♣ ♥ ♦
  def rank2( h: Hand ) = {
    val cs = Hand("9♥|9♦|T♦|T♣|J♠|Q♦|K♠").cards
//    val cs = Hand("8♦|9♠|T♦|T♣|J♣|Q♣|K♣").cards

//    println(cs)
    val str8s = genStr8s( cs )
//    println("str8s: " + str8s )
    cs.sorted.reverse.sliding(5).collect{ case x if cardsAreSequential(x) => x }.toList.partition( areFlushed(_) ) match {
      case x => println(x)
      case (List(),y) => ("Straight", y.head)
      case (x,_) => 
        x.head.head.rank match {
          case Rank.Ace => ("Royal", x.head )
          case _ => ("StraightFlush", x.head )
        }
 
    }

  }

  def rank( h: Hand ) = {
//    val cs = h.cards.sorted
    val cs = Hand("9♠|9♣|T♠|T♣|J♣|Q♣|K♣").cards.reverse
                 //♠♣♥♦

    var highcard = cs.head

    val str8_prep = { if ( highcard.rank == Rank.Ace ){ highcard +: cs } else { cs } }
//    val wheel_str8 = css.map( _.hashCode ).sum 

      /** reverse gives left to right scan; means we find highest straight first (more often than not) 
       ** wheel str8 is on the far right of the map */
      val str8_hands = str8_prep.sliding(5).toList.reverse
      val str8_check = str8_hands.map( cardsAreSequential( _ )  )

      println("str8_check: " + str8_check )
      /** not strictly idiomatic I'm thinking, but slightly more readable **/
      val hc = str8_check.indexOf( true )
      val (hrank, str8_val) = str8_hands.lift(hc) match {
        case Some(x) => 
          /** if all 5 suits are the same then the groupedBy size is unity. i.e., 5/5 **/
          val flsh = x.map( _.suit ).groupBy(identity).mapValues(_.size).size == 1
          val hr = flsh match { 
            case true => 
              x.head.rank match {
                case Rank.Ace => "Hand.Rank.RoyalFlush"
                case _ => "Hand.Rank.StraightFlush"
              }

            case false => "Hand.Rank.Straight"
          }

          (hr, x.head)

        case _ => (None, None)

      } 
      println( hrank, str8_val )
    val ranks = { cs.map( _.rank ).groupBy(identity)  }
    val flushSuit = { cs.map( _.suit ).groupBy(identity).mapValues(_.size).filter(_._2 >= 5).keys.headOption }
    val (flushed, fhc) = flushSuit match {
      case Some( x: Suit ) => ( "Hand.Rank.Flush", highcard )
      case _ => ( None, None )
    }

  //  }
    //val ( fours, threestwos ) 
    val settishs  = ranks.mapValues(_.size).filter(_._2 >= 2)
    val rgrps = settishs.groupBy( _._2 )
    
    val bar = rgrps.keys.toList.sorted.reverse match {
      case List() => ("Hand.Rank.HighCard", highcard )
      case x: List[Int] if x.head == 4 => ("Hand.Rank.FourOfAKind",rgrps.lift(x.head).head.head._1)
      case x: List[Int] if x.head == 3 => 

        val topthree = rgrps.lift(3).head.toList(0)._1
        val threes = rgrps.apply( x.head )


        val nar = threes match {
          case x if x.size == 1 => 
            val twos = rgrps.lift(2) 
            twos match {
              case Some(x) => ("Hand.Rank.FullHouse", (topthree,  x.keys.toSeq.maxBy( _.value ) ) )
              case _ => (None,None)
            }

          case x if x.size == 2 => 
              val lowerthree = threes.tail.head._1
             ("Hand.Rank.FullHouse", (topthree, lowerthree))
          case _      => None
        }

        


      case x: List[Int] if x.head == 2 => 
        rgrps.apply(2).size match { 
          case x: Int if x == 1  => ("Hand.Rank.Pair", rgrps.apply(2).toList(0)._1 )
          case x: Int if x > 1  => ("Hand.Rank.TwoPair", rgrps.apply(2).toList.sortWith( _._1 > _._1 ).take(2).map( _._1 ) )
          case _ => (None,None)
          
        }

      case _ => None
    }

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

    println( Math.nck( 52, 2 ) )

    val h1 = dealer.deal( 7 ) 

println(    Hand.rank2( h1 ) )


//   val ranksOne = for { c <- cards; b <- cards if (c.rank == b.rank && c.suit != b.suit ) || c.rank != b.rank  } yield { val h = new Hand( List(c,b) ); h.sort(); println(h) ; h } //(c.hashCode, b.hashCode, h.hashCode, h)  } 
//    println( for ( c1 <- csssplit(0); c2 <- csssplit(1) ) yield { new Hand( c1,c2 ) } )

  }
}

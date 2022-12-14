package pcalc
import pcalc.types._
import enumeratum.{ Enum, EnumEntry }
import enumeratum.values.{ IntEnum, IntEnumEntry }

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

sealed abstract class HandRank( val value: Int ) extends IntEnumEntry with Ordered[HandRank] with Product with Serializable {
  override def compare( that: HandRank ) = { this.value - that.value }
  def equals (that: HandRank ) = { this.value == that.value }
}

object HandRank extends IntEnum[HandRank] {

  val values = findValues

  case object Unknown       extends HandRank(0)
  case object HighCard      extends HandRank(1)
  case object Pair          extends HandRank(2)
  case object TwoPair       extends HandRank(3)
  case object ThreeOfAKind  extends HandRank(4)
  case object Straight      extends HandRank(5)
  case object Flush         extends HandRank(6)
  case object FullHouse     extends HandRank(7)
  case object FourOfAKind   extends HandRank(8)
  case object StraightFlush extends HandRank(9)
  case object RoyalFlush    extends HandRank(10)
 
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

  def cards = _cards.toList

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

  def cardsAreSequential( cs: Cards ) = {
    cs.sliding(2).map{ 
      case Seq(x,y) => Rank.valuesToIndex( y.rank ) - Rank.valuesToIndex( x.rank )
    }.toList match {
      case List(-12,1,1,1) | List(1,1,1,1) => true
      case _ => false
    }

  }

  def areFlushed( cs: Cards ): Boolean = {
    cs.tail.count( _.suit == cs.head.suit ) == cs.length - 1
  }

  def genStr8s( cs: Cards ): List[Cards] = {
   
    if( cs.length < 5 ) { return List() } 
 
    val rankMap = cs.sorted.reverse.groupBy( _.rank )

    // ranksOne -- cards for which there is only one possibility
    val ranksOne = rankMap.filter( _._2.size == 1 ).values.flatten.toList
    // ranksMany -- cards which have duplicate rank
    val ranksMany = rankMap.filter( _._2.size > 1 )

    // ranksManyCount -- how many Duplicate card groups?
    val ranksManyCount = ranksMany.keys.size

    val totalDupedCards = ranksMany.values.flatten.size 

    // no dupes, one set of two, two sets of two, or one set of three, but no more
    val canBeStr8 = (ranksManyCount, totalDupedCards) match {
      case (1,2) | (2,4) | (1,3) | (0,0) => true
      case _ => false
    }

    val str8s = canBeStr8 match {

      case true =>

        if( ranksManyCount == 0 ){
          val css = cs.last match {
            case x if x.rank == Rank.Ace => x +: cs 
            case _ => cs
          } 
          css.sliding(5).collect{ case x if cardsAreSequential(x) => x }.toList
        }else{

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

      }
      

      case false =>
        List()

     }


  return str8s

} 

  def isUnknown( hr: (String,Any) ): Boolean = {
    return hr._1 == "Unknown"
  }

  def getHighest( h1: (HandRank, Cards), h2: (HandRank, Cards) ): (HandRank, Cards) = {
    return if( h1._1 > h2._1 ) h1 else h2
    
  }

  //♠ ♣ ♥ ♦
  def rank2( h: Hand ): (HandRank,Cards) = {

    val cs = h.cards

    import scala.collection.immutable.ListMap
    import HandRank._

    val str8s = genStr8s( cs ).partition( areFlushed(_) ) 

    var resHand = str8s match {
      case (List(),List()) => (Unknown, List())
      case (List(),x) => (Straight, x.last)
      case (x,_) =>
        x.last.last.rank match {
          case Rank.Ace => (RoyalFlush, x.last.toList )
          case _ => (StraightFlush, x.last )
        }
    }

    if ( resHand._1 <= Straight ){

      val sets = ListMap(cs.groupBy( _.rank ).filter( _._2.size >= 2 ).groupBy( _._2.size ).toSeq.sortWith( _._1 < _._1 ):_*)
   
      val sets_flat = sets.map( _._2 ).map( _.toSeq.sortWith( _._1 < _._1 ).flatMap( _._2 ) ).flatten.toList

      resHand = sets.keys.toList.sorted match {

        case List()    => getHighest( ( HighCard, cs.takeRight(5).sorted ), resHand )  
        case List(4) | List(_,4)  => (FourOfAKind, sets_flat.takeRight(4))
        case List(2) => 
          val twos = sets_flat.takeRight(4) 
          twos.size match {
            case 2 => getHighest( resHand, ( Pair, Dealer.ensureRemoved( cs, twos ).takeRight(3) ++ twos ) )
            case 4 | 6 => getHighest( resHand, ( TwoPair, Dealer.ensureRemoved( cs, twos ).takeRight(1) ++ twos ) ) //getHighest( resHand, (TwoPair, twos) ) // or a str8
         }

        case List(2,3) | List(3) => 
          val threes = sets_flat.takeRight(5)
          threes.size match {
            case 3 => getHighest( resHand, ( ThreeOfAKind, Dealer.ensureRemoved( cs, threes ).takeRight(2) ++ threes ) )
            case 5 => (FullHouse, threes )
         }
       
      }

    }

    if( resHand._1 < FullHouse ){
      resHand = cs.groupBy( _.suit ).filter( _._2.size == 5 ).map( _._2 ).flatten match {
        case List() => resHand
        case x => ( Flush, x.toList.sorted )
      }
    }

    resHand

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
  import HandRank._

  def main( args: Array[String] ): Unit = {

    val itn = args.lift(0).getOrElse("10").toInt
    println( itn )

    val dealer = new Dealer()
    var h: Hand =  Hand("T♣") // needed because I don't fully understand 'var' usage
    val num_players = 2
    


    for ( i <- 1 to itn ) {

      dealer.shuffleDeck()

      val ps = for ( i <- 1 to num_players ) yield {
        val h = dealer.deal(2) 
        dealer.ensureRemoved( h )
        h
      }           

      h = dealer.deal( 3 )
      dealer.ensureRemoved( h.cards )
      printf("Flop: %s -> %s\n", h.sorted, Hand.rank2( h ) )

      println("HoleCards: " + ps.map( _.sorted ) ) 
      println("PlayerHands: " + ps.map( (hp: Hand) => ( Hand.rank2( Hand.merge(h,hp).sorted ) ) ) )
      // println(    Hand.rank2( h ) )

      val t = dealer.deal(1)
      h.merge(t)
      println("Turn: " + h + ", Hs: " + ps.map( Hand.merge(h, _).sorted ).map( Hand.rank2( _ ) ).map( _._1 )) 

      val r = dealer.deal(1)
      h.merge(r)
      println("River: " + h + ", Hs: " + ps.map( Hand.merge(h, _).sorted ).map( Hand.rank2( _ ) ) ) //.map( _._1 )) 

      println("")

      dealer.newDeck() 
    }
  }
}

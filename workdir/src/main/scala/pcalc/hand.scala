package pcalc.hand

import pcalc.card.{ Card, Rank, Suit }
import pcalc.card.plural.Cards

import pcalc.dealer.{ Dealer } 

import enumeratum.values.{ IntEnum, IntEnumEntry }

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


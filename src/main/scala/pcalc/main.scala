package pcalc
import pcalc.types._

package object types {

  type DeckSeqTuples = Seq[(Char,Char)]
  type Deck = Seq[Card]

}


class Hand() {
 
  private val _n: Int = 2
  private var _h: Deck = null

  def this( d: Deck, n: Int ) = { 
    this()
    this._h = d.take(n) 
  } 

  def h = _h

  override def toString() = { 
    "[" + _h.mkString("|") + "]"
  }

}


object Dealer {

  var deck = CardPivot.DeckOfCards
  
  def shuffle( d: types.Deck = this.deck): Deck = {
    scala.util.Random.shuffle( d )
  } 

  /* external state*/
  def deal( d: Deck, n: Int ): Hand = {
    new Hand( d, n )
  }


}

object CardPivot {

  val CardStringSuits = "♠♣♥♦"
  val CardStringFaces = "23456789TJQKA"
  val CardTupleSeq: DeckSeqTuples = for { f <- CardStringSuits; s <- CardStringFaces } yield { (s,f) }   
  val DeckOfCards: Deck = for { ct <- CardTupleSeq } yield { new Card(ct) }

  def tupleToInt( t: (Char,Char) ): Int = {
    return CardTupleSeq.indexOf(t)
  }

  def tupleToString( t: (Char,Char) ): String = {
    val (x,y) = t
    return x + "" + y.toLower
  }

  def stringToTuple( s: String ): (Char,Char) = {
    return (s(0),s(1).toLower)
  }

  def stringToInt( s: String ): Int = {
    return tupleToInt( stringToTuple( s ) )
  }

  def intToString( i: Int ): String = {
    return tupleToString( intToTuple(i) )
  }

  def intToTuple( i: Int ): (Char,Char) = {
    return CardTupleSeq(i) 
  }
}

class Card() { 

  private var f: Int = 0
  private var s: Int = 0
  private var _i: Int = 0
  private var _tuple: (Char, Char) = ('A','s')
  private var _srep: String = ""

  private var _isFaceUp = false

  def isFaceUp = _isFaceUp
  def isFaceUp_= ( b: Boolean ) = { _isFaceUp = b } 

  def tuple = _tuple
  def tuple_= ( t: (Char,Char) ) = { _tuple = t }

  def srep = _srep
  def srep_= ( s: String ) = { _srep = s }

  def i = _i
  def i_= ( i: Int ) = { _i = i }

  def this( s: String ) = {
    this()
    this._srep = s
    this._tuple = CardPivot.stringToTuple(s)  
  }

  def this( t: (Char, Char)  )  = { 
    this()
    this._srep = CardPivot.tupleToString(t)
    this._tuple = t
  }

  def this( i: Int ) = {
    this()
    this._srep = CardPivot.intToString(i)
    this._tuple = CardPivot.intToTuple(i)
    this._i = i
  }
    
  override def toString(): String = {
    if (this._isFaceUp) { 
      return this._srep
    } 
      return "🃧"
  }

}

object CardMaths {
  def fact(n: BigInt): BigInt = {
    if (n == 0) { return 1 } else { n * fact(n-1) } 
  }

  def factFold(n: BigInt): BigInt = { 
    val one: BigInt = 1
    return (one to n).foldLeft(one)( (a,b) => (a*b) )
  }

  def pnk( n: BigInt, k: BigInt ): BigInt = {
    if ( k > n ) { return 0 } else {
     return fact(n) / fact(n-k) 
    }
  }

  def nck( n: BigInt, k: BigInt ): BigInt = {
    if ( k > n ) { return 0 } else {
     return pnk(n, k) / fact(k) 
    }
  }

  /** t: total number of available cards **/
  def choosePocketPair( t: Int ): BigInt = {
    return nck( t, 2 ) 
  }

  def getPocketProbability( avail: Int, ways1: Int, ways2:Int ): Double = {
    return (ways1.toDouble / avail * ways2.toDouble / (avail-1.0)) * 100
  }

  final val PROB_POCKET_SUITED_ANY = (52, 52, 3) // (TOTAL_NO_OF_CARDS, TOTAL_NO_OF_CARDS, NO_OF_CARDS_PER_SUIT-1

  def getPocketProbability( cfreqs: ( Int, Int, Int ) ): Double = {
    val avail = cfreqs._1
    val ways1 = cfreqs._2
    val ways2 = cfreqs._3

    return getPocketProbability( avail, ways1, ways2 )
  }

  final val TOTAL_NO_OF_CARDS = 52
  final val NO_OF_SUITS = 4
  final val NO_OF_EACH_CARD = NO_OF_SUITS
  final val NO_OF_CARDS_PER_SUIT = 13
 
  def getPocketSuitedProbability(): Double = { 
    return getPocketProbability( TOTAL_NO_OF_CARDS, TOTAL_NO_OF_CARDS, NO_OF_CARDS_PER_SUIT-1) 
  } 

  def getPocketPairProbability(   ): Double = {
    return getPocketProbability( TOTAL_NO_OF_CARDS, TOTAL_NO_OF_CARDS, NO_OF_EACH_CARD-1 ) 
  } 

/*
  P(A)  = 0.666666 
  P(~A) = 0.333333

  if (A) { 0.5 } else { 0.0 }
*/
  final val TOTAL_HANDS_7: Int = 133784560
  final val RATIO_HANDS_POCKET_PAIR: Double = getPocketPairProbability()
  final val RATIO_HANDS_POCKET_FLUSHDRAW: Double = 0.06862745098039215
//  final val RATIO_HANDS_POCKET_

  def main( args: Array[String] ): Unit = { 
    var full_deck = Dealer.shuffle(CardPivot.DeckOfCards)
//    var (hand, rest) = (full_deck.take(2), full_deck.drop(2))
    var (hand, rest) = ( Dealer.deal( full_deck, 2 ), full_deck.drop(2) )
    hand.h(0).isFaceUp = true
    hand.h(1).isFaceUp = true 
    printf("hand: %s, rest: %s\n", hand, rest)

  }
}


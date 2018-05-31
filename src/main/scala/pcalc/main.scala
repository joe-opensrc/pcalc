package pcalc
import pcalc.types._

package object types {

  type DeckSeqTuples = Seq[(Char,Char)]
  type Deck = Seq[Card]
  type Board = Hand


}

package holdem.hands {
  object types {
    val HIGH_CARD       = 0 // a.b.c.d.z
    val PAIR            = 1 // a.a.b.c.e
    val TWO_PAIR        = 2 // a.a.b.b.c
    val THREE_OF_A_KIND = 3 // a.a.a.b.c
    val STRAIGHT        = 4 // a.b.c.d.e
    val FLUSH           = 5 // A.A.A.A.A
    val FULL_HOUSE      = 6 // a.a.a.b.b
    val STRAIGHT_FLUSH  = 7 // A.B.C.D.E
    val ROYAL_FLUSH     = 8 // V.W.X.Y.Z

  }
}


object HandPivot {

  def handType( h: Hand ): Int = {
    /** flush **/
    println( h.h.groupBy( _.schr ).mapValues(_.size).filter( _._2 >= 5 ))

//    println( h.h.groupBy( _.fchr ).mapValues(_.size).filter( _._2 >= 2 ))
    0
  }

  def handType( b: Board, h: Hand ): Int = {
    h.merge(b)
    h.sort()
    println(h)
    println( h.h.groupBy( _.fchr  ).mapValues(_.size).filter( _._2 >= 2 ))
    0
  }

  /** TODO: length must be 5, or throw error **/
  def detectStraight( h: Hand ) {
     
  }

  def parseHand( h: Hand ): List[(Char,Int)] = {
    val d = h.h.sorted
    //♠♣♥♦    
    List(('♠',3))
  }

}

/** quite possibly make a HandFactory to implement game specific functions, i.e., hand ranking, number of cards per hand, etc **/
class Hand() {
 
  private val _n: Int = 2
  private var _h: Deck = Seq() 

  def this( d: Deck, n: Int ) = { 
    this()
    this._h = d.take(n) 
  } 

  def this( d: Deck ) = {
    this()
    this._h = d
  }

  def this( s: String ) = {
    this()
    val css = s.split('|')
    for ( c <- css ) { this._h = this._h :+ new Card(c)   }
  }

  def sort(): Unit = {
    this._h = this._h.sorted
  }

  def merge( h: Hand ): Unit = {
    this._h = this._h ++ h.h
  }

  /* don't really know what I'm doing here */
  def rank( ) = {
    //if ( h.lengh < 5 ){ 
    //  throw Error()
    //}
    val h = this._h.sorted
    h
    
  }

  def h = _h

  def length(): Int = {
    return this._h.length
  }

  override def toString() = { 
    "[" + _h.mkString("|") + "]"
  }

}


object Dealer {

  final val START_UNSHUFFLED: Int = 0
  final val START_SHUFFLED: Int = 1

  def newDeck: Deck         = { CardPivot.DeckOfCards }
  def newDeckShuffled: Deck = { this.shuffle( CardPivot.DeckOfCards ) } 

  def shuffle( d: Deck ): Deck = {
    scala.util.Random.shuffle( d )
  } 

  def deal( d: Deck, n: Int ): (Hand,Deck) = {
    ( new Hand( d.take(n) ), d.drop(n) )
  }

  /** needs to be admin only, made efficient **/
  def removeHand( d: Deck, h: Hand): Deck = {
    var r: Deck = d
    for ( c <- h.h ){ r = r.filterNot( _.equals(c) ) }
    return r
  }

  /** TODO: define sort types **/
  def sort( d: Deck ): Deck = {
    d 
  }

  def instance( start_type: Int = START_UNSHUFFLED ) = {
    new Dealer( start_type )
  }


  def getHandType( h: Hand )  = {}

}

class Dealer() {  

  private var _deck: Deck = Dealer.newDeck

  def this( start_type: Int = 0 ) = {
    this()
    start_type match {
      case Dealer.START_SHUFFLED => 
        this._deck = Dealer.newDeckShuffled 
      case _ => 
    }
  }

  def shuffle(): Unit = {
    this._deck = Dealer.shuffle( _deck ) 
  }

  def reset(): Unit = {
    this._deck = Dealer.newDeck
  }

  def deck = _deck
//  def deck_= ( d: Deck ) = { this._deck = d }

  def deal( n: Int ): Hand = {
    val h = new Hand( this._deck.take(n) )
    this._deck = this._deck.drop(n) 
    return h
  }

  def removeHand( h: Hand ): Unit = {
    this._deck = Dealer.removeHand( this._deck, h )
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

  def faceValue( c: Char ): Int = {
    return CardStringFaces.indexOf( c ) + 2
  }

  def faceToChar( i: Int ): Char = {
    CardStringFaces(i-2)
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

class Card() extends Ordered [Card] { 

  private var _f: Int = 0
  private var _s: Int = 0
  private var _i: Int = 0
  private var _tuple: (Char, Char) = ('A','s')
  private var _srep: String = ""

  private var _isFaceUp = true //false

  def isFaceUp = _isFaceUp
  def isFaceUp_= ( b: Boolean ) = { _isFaceUp = b } 

  def tuple = _tuple
  def tuple_= ( t: (Char,Char) ) = { _tuple = t }

  def srep = _srep
  def srep_= ( s: String ) = { _srep = s }

  def i = _i
  def i_= ( i: Int ) = { _i = i }

  def f = _f
  def fchr = CardPivot.faceToChar(_f)

  def s = _s
  def schr = this._srep(1)

  def this( s: String ) = {
    this()
    this._srep = s
    this._tuple = CardPivot.stringToTuple(s)  
    this._f = CardPivot.faceValue( this._tuple._1 )
  }

  def this( t: (Char, Char)  )  = { 
    this()
    this._srep = CardPivot.tupleToString(t)
    this._tuple = t
    this._f = CardPivot.faceValue( this._tuple._1 )
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

  override def equals( comparator: Any ): Boolean = comparator match {
    case comparator: Card => comparator.isInstanceOf[Card] && this.srep == comparator.srep
    case _ => false
  }

  override def compare( comparator: Card ): Int = {
    val lf = this._f
    val rf = comparator.f 

    if ( lf == rf ) { 
      return 0 
    } else { 
      if ( lf > rf ) {
        return 1
      } else {
        return -1
      }

    }
 
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

  def factors(num: BigInt) = {
    val sr = scala.math.ceil( scala.math.sqrt(num.toDouble) ).toInt
    ( 1 to sr ).filter { divisor =>
      num % divisor == 0
    }
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


  object holdem {
    def rank( h: Hand ){
      if ( h.length < 5 ){ 
        println("Hand needs to at least 5 cards")
      }else{
        
        // sort hand, 
 
      }

    }
  }




  def main( args: Array[String] ): Unit = { 

//println( CardMaths.factors( CardMaths.nck( 52, 5 )) )

//println( scala.math.sqrt( CardMaths.nck( 52, 5 ).toDouble ))
/** get hand, get sum, if straight compare indeces **/
val sum_of_straights = Vector( 14, 2, 3, 4, 5 ).sum +: ( for ( i <- 2 to 10 ) yield { var x = for ( j <- 0 to 4 ) yield { i+j } ; x.sum } )  
println( sum_of_straights )
    var dealer = Dealer.instance(Dealer.START_SHUFFLED)

//    for ( c <- dealer.deck ) { printf("%d,", CardPivot.cardToInt

    // "♠♣♥♦"
//    var h1 = new Hand("A♠|K♦")
//    println( "h1: " + HandPivot.parseHand(h1) )
//    var h2 = new Hand("A♣|A♥")
//    h1.sort


    val NUM_PLAYERS = 9 
    val NUM_ROUNDS = 1000

  for ( c <- 1 to NUM_ROUNDS ) { 

 //     var h1 = new Hand("A♠|K♦")
//      h1.sort
//      dealer.removeHand( h1 ) 
//    dealer.removeHand( h2 )
   
//    dealer.shuffle()
   
//    val other = for ( i <- 1 to NUM_PLAYERS) yield { val h = dealer.deal(2); h.sort(); h }    
      var b: Hand = dealer.deal(7)
      b.sort() 
      println(b)
//      println( HandPivot.handType(b,h1) )
      HandPivot.handType( b )
       
//    printf("%s  %s  ", h1, b ) 
//    val other = List()
//    for ( h <- other ){ printf("%s ", h  ) } //dealer.deck)  

//    println()

    dealer.reset()
    dealer.shuffle()
//    dealer.removeHand(h1)
//    printf("%s,%s,%s\n", dealer.deal(2), dealer.deal(3), dealer.deck ) 
  }
  }
}


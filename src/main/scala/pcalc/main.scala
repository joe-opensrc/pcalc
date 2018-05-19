package pcalc

class CardPivot() {


  val CardStringSuits = "HCDS"
  val CardStringFaces = "23456789TJQKA"
  val CardTupleSeq: Seq[(Char,Char)] = for { f <- CardStringSuits; s <- CardStringFaces } yield (s,f)   

  def tupleToInt( t: (Char,Char) ): Int = {
    return CardTupleSeq.indexOf(t)
  }

  def intToTuple( cv: Int ): (Char,Char) = {
    return CardTupleSeq(cv) 
  }

  def tupleToString( t: (Char,Char) ): String = {
    val (x,y) = t
    return x + "" + y.toLower
  }

}

class Card( var srep: String ) { 

  private var f: Int = 0
  private var s: Int = 0

/* 
  def this( _f: Int, _s: Int )  = { 
    this("")
    this.srep = "Tc" 
  }

//  def getFace(): Int = {  }
*/    
}



/*
class Card( private var _srep: String ){

  private var _v: Int = 0
  private var _s: Int = 0

  def srep = _srep
  def srep_=( s: String ) = { _srep = s }

}
*/

object maths {
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

  def main( args: Array[String] ) = { 
  //var cp = new CardParser()
    var c = new Card("As")
    var cp = new CardPivot()

    println(cp.tupleToString(cp.intToTuple(39)))

  //val s = args.toList(0)
  //val x = s.toInt
  //val x = BigInt(s)
  //printf("P(pair) == %s\n", choosePocketPair( x ) )
  //printf("P(pair) == %s\n", getPocketProbability( 52, 14, 13 )) //14.0/52.0 * ( 13.0/51.0 ) )
  //println(getPocketPairProbability())
  //println(getPocketSuitedProbability())
  //println(nck(52,2))

  }
}

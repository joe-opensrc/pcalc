package pcalc

sealed abstract class Rank( val c: Char, val order: Int ) extends Ordered[Rank] {

  def toChar = c 
  def toInt = order
  def value = order
 
  def compare( that: Rank ) = { this.order - that.order }
  def equals( that: Rank ) = { this.value == that.value }

}

//sealed abstract class Suit( val s: Char, val order: Int ) extends Ordered[Suit] {
//
//  def toChar = s
//  def toInt = order
//  def value = order
//  
//  def compare( that: Suit ) = { this.order - that.order }
//  def equals( that: Suit ) = { this.value == that.value }
//
//}

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


object Main {
  def main( args: Array[String] ): Unit = {
    val n = Ten
    val m = Ten

    println( n == m )
    println(n.value)
  }
}

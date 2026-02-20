package pcalc

import pcalc.card.{ Card, Rank, Suit } 
import pcalc.card.plural.{ Cards }
import pcalc.hand._
import pcalc.dealer.{ Dealer }


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

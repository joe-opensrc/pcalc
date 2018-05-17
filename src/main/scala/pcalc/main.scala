package pcalc

object maths extends App {
  def fact(n: BigInt): BigInt = {
    if (n == 0) { return 1 } else { n * fact( n - 1) } 
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

  printf( "fact(52) == %d\n", fact(52) )
  printf( "pnk(5,2) == %d\n", pnk(5,2) )
  printf( "nck(52,2) == %d\n", nck(52,2) )

}



package pcalc

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

  def main( args: Array[String] ) = { 
    val s = args.toList(0)
    val x = BigInt(s)
    printf("fact(%d) == %d\n", x, factFold(x))
  }

}


package algos.math

object NumberTheory {

	//Finding prime number by Sieve of Eratosthenes
	def sieveOfEratosthenes(n:Int) = {
		val num = 2 to n 
		val isPrime = scala.collection.mutable.BitSet(num: _*)
		for {
			p <- num takeWhile(i => i * i <=n)
			if( isPrime(p))
		} {
			isPrime --= p * p to n by p
		}	
		isPrime
	}

	//Uses Euclid's GCD algorithm
	def gcd(a:Int,b:Int):Int = (a, b) match {
    case _ if a < 0 => gcd(-a, b)
    case _ if b < 0 => gcd(a, -b)
    case (_, 0) => assume(a!=0); a
    case _ => gcd(b, a%b)
  }

   def lcm(a: Int, b: Int) = a/gcd(a,b) * b
}

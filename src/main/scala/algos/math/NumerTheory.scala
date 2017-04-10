object NumberTheory {

	//Finding prime number by Sieve of Eratosthenes
	def sieveOfEratosthenes(n:Int) = {
		val num = 1 to n 
		val isPrime = scala.collection.mutable.BitSet(num: _*)
		for {
			p <- num takeWhile(i => i * i <=n)
			if( isPrime(p))
		} {
			isPrime --= p * p to n by p
		}	

		isPrime.toImmutable
	}



}
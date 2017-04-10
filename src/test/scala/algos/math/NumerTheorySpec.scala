package algos.math

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class NumberTheorySpec extends FlatSpec  {

  import algos.math.NumberTheory._

  "Sieve of Eratosthenes" should "return expected prime numbers" in {
    assert(sieveOfEratosthenes(120).toList == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113))
  }

  "GCD algorithm" should "return expected result" in {
  	assert(gcd(15,25) == 5)
  }

  "LCM algorithm" should "return expected result" in {
  	assert(lcm(12,80) == 240)
  	assert(lcm(2,lcm(3,4)) == 12)
  }
}
package algos.bitwise

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class BitwiseSpec extends FlatSpec  {


   import algos.bitwise.Bitwise._

  "Convert case" should "match expected result" in {
    assert(convertCase('a') =='A')
  }

 "Check Even" should "match expected result" in {
    assert(checkEven(12) == true)
    assert(checkEven(11) == false)
  }

  "check nth bit" should "match expected result" in {
    assert(isNthBitSet(122,3) == true)
    assert(isNthBitSet(10,2) == false)
  }

  "set nth bit" should "match expected result" in {
    assert(setNthBit(10,2) == 14)    
  }

  "Unset nth bit" should "match expected result" in {
    assert(unSetNthBit(14,2) == 10)    
  }

   "Toggle nth bit" should "match expected result" in {
    assert(toggleNthBit(10,2) == 14)    
  }

   "turn off right most 1 bit" should "match expected result" in {
    assert(turnOffRightMost1Bit(10) == 8)    
  }

   "turn on right most 0 bit" should "match expected result" in {
    assert(turnOnRightMost0Bit(10) == 11)    
  }

}
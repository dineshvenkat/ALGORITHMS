package algos.dp

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class DynamicProgrammingSpec extends FlatSpec  {

  import algos.dp.DP._

  "LongestCommonSeq" should "match expected result" in {
    val a = "ABCDGH".toList
    val b = "AEDFHR".toList
    assert(LargestCommonSeq(a,b).mkString=="ADH")
  }

  "knap sack" should "match expected result" in {
    val v = List(60, 100, 120)
    val w = List(10, 20, 30)
    assert(knapSack(50,0,v.toArray,w.toArray,scala.collection.mutable.Map[Int,Int]()) == 220)
  }

  "Changing coin " should "match expected result" in {
    val input = List(9, 6, 5, 1)
    assert(coinChange(input,11,scala.collection.mutable.Map(0->List())).mkString == "65")
  }

  "String Edit distance " should "match expected result" in {
    val s1 = "KITTEN"
    val s2 = "SITTING"
    assert(editDist(s1.toList,
      s2.toList,
      (x:Char) => 1,
      (x:Char) =>1,
      (x:Char,y:Char) => if(x == y) 0 else 1
    )	 == 3)
  }

  "Maximum sum in a given sequence" should "match expected result" in {
    val a = List(-2, -3, 4, -1, -2, 1, 5, -3)
    assert(maxSumInSeq(a) == 7)
  }

  "Get the longest increasing sequence" should "match expected result" in {
    val input = List(10, 22, 9, 33, 21, 50, 41, 60, 80)
    assert(longestIncreasingSeq(input) == List(10, 22, 33, 50, 60, 80))
  }

  "Get most frequent word in a string" should "match expected result" in {
    val input = "This is a test this may not be this is this"
    assert(mostFrequentWords(input,1).head == "this")
  }

  "Minimum number of jumps required" should "match expected result" in {
    val input = List(2,3,1,1,2,4,2,0,1,1)
    assert(minimumJump(input) == 4 )
  }

  "Number of encodings for string" should "match expected result" in {
    val input = "12321"
    val input1 = "104"
    assert(numEncoding(input.toList,scala.collection.mutable.Map[String,Int]()) == 6)
    assert(numEncoding(input1.toList,scala.collection.mutable.Map[String,Int]()) == 3)
  }

  "Stacking the box" should "match expected result" in {
    val input = List((4,6,7),(1,2,3),(4,5,6),(10,12,32))
    assert(stackBox(input) == 60 )
  }

  "Subset sum" should "match expected result" in {
    val input = List(3, 34, 4, 12, 5, 2)
    assert(subSetSum(input,9,scala.collection.mutable.Map[(List[Int],Int),Boolean]()) == true )
  }

}

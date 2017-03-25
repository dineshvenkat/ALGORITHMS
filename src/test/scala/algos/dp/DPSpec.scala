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

}

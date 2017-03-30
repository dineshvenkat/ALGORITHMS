package algos.string

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class StringAlgoSpec extends FlatSpec  {

  import algos.string.StringAlgos._

  "Get most frequent word in a string" should "match expected result" in {
    val input = "This is a test this may not be this is this"
    assert(mostFrequentWords(input,1).head == "this")
  }


}

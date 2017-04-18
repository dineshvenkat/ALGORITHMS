package algos.CI_QUESTIONS

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class SolutionSpec extends FlatSpec  {

   import algos.CI_QUESTIONS.Solution._
  "1.1 Test unique char in string" should "return expected result" in {
  	assert(uniqueChar("this") == true)
  	assert(uniqueChar("wow") == false)
  	//assert(uniqueCharBitwise("this") == true)
  	assert(uniqueCharBitwise("bib") == false)
  }

 "1.3 remove duplicate char in string" should "return expected result" in {
  	assert(removeDups("mama") == "ma")
  	assert(removeDups("this") == "this")
  }

  "1.4 anagram check on two strings" should "return expected result" in {
  	assert(checkAnagram("debit card","bad credit") == true)
  	assert(checkAnagram("debit card","good credit") == false)
  }

  "1.5 replace space with %20 in a string" should "return expected result" in {
  	assert(replaceSpace("debit card") == "debit%20card")
  	
  }

  "1.5 string rotation check" should "return expected result" in {
  	assert(rotateCheck("apple","plea") == true)
  	assert(rotateCheck("apple","pleal") == false) 
  }

  "2.1 remove duplicates from an unsorted linked list" should "return expected result" in {
  	assert(removeDupsInList(List("apple","plea","apple","abcd")) == List("apple","plea","abcd"))
  }	

  "2.2 find nth element in List" should "return expected result" in {
  	assert(findNthElemInList(List("apple","plea","apple","abcd","test"),3) == Some("apple"))
  	assert(findNthElemInList(List("apple","plea","apple","abcd"),8) == None)
  }

  "2.3 delete matching element in List" should "return expected result" in {
  	assert(deleteElemInList(List("apple","plea","apple","abcd","test"),"apple") ==
  								 List("plea","abcd","test"))
  	assert(deleteElemInList(List("apple","plea","apple","abcd","test"),"orange") ==
  								 List("apple","plea","apple","abcd","test"))
  }

  "2.4 Adding numbers in two List" should "return explected result" in {
  	def ch(s:String):List[Int] = s.toList.map(x => Integer.parseInt(x.toString))
  	val l1 = ch("315")
  	val l2 = ch("592")
  	val l3 = ch("41511")

  	assert(addListNums(l1,l2) == List(8,0,8))
  	assert(addListNums(l3,l1) == List(7,2,0,2,1))
  }

  "3.1 multi stack in a Array" should "return expected results" in {
  	val multiStack = new ThreeStackWithArray[Int](9,0)
  	assert(multiStack.pushTo(1,3).mkString == Array(0,0,0,0,0,0,1,0,0).mkString)
  	assert(multiStack.pushTo(2,1).mkString == Array(2,0,0,0,0,0,1,0,0).mkString)
  	val r = multiStack.popFrom(3)
  	assert(r._1 == 1)
  	assert(r._2.mkString == Array(2,0,0,0,0,0,0,0,0).mkString)
  }

  "3.2 Special stack to support min inaddtion to push and pop" should "return expected results" in {
  	val specialStack = new SpecialStack[Int]()
  	val s1 = specialStack.push(3)
  	assert(s1.toString == "(3,3)")
  	val s2 = s1.push(5)
  	assert(s2.toString == "(3,5),(3,3)")
  	assert(s2.min == 3)
  	assert(s2.pop.toString == "(5,(3,3))")
  }

  "3.3 Stack of Stacks" should "return expected result" in {
  	val ss = new SetOfStacks[Int](3)
  	val s1 = ss.push(1)
  	val s2 = s1.push(2)
  	val s3 = s2.push(3)
  	val s4 = s3.push(4)
  	assert(s4.toString == List(List(4), List(3, 2, 1)).toString)
  	val s5 = s4.pop
  	assert(s5._1 == 4)
  	assert(s5._2.toString == List(List(3,2,1)).toString)
  }

  "4.1 isBalanced tree" should "return expected result" in {
    val n1 = Node(None,1,None)
    val n2 = Node(None,2,None)
    val n3 = Node(None,3,None)
    val n4 = Node(None,4,None)
    val n5 = Node(Some(n1),5,Some(n2))
    val n6 = Node(Some(n3),6,Some(n4))
    val n7 = Node(Some(n3),7,Some(n6))
    val n8 = Node(None,8,Some(n1))
    val n9 = Node(None,9,Some(n8))
    assert(isBalanced(Some(n7)) == true)
    assert(isBalanced(Some(n9)) == false)
  }

  "4.2 checking connected nodes" should "return expected results" in {
    val g = new Graph(5,true)
    g(0->1) = 10
    g(1->3) = 20
    g(2->3) = 30
    g(3->4) = 25
    assert(Graph.isConnected(g,0,4) == true)
    assert(Graph.isConnected(g,2,1) == false)
   
  }

  "4.3 create balanced tree from list" should "return expected results" in {
    val in1 = List(1,2,3,4,5,6,7)
    val in2 = List(1,2)    
    val res1 = Some(Node(Some(Node(Some(Node(None,1,None)),2,Some(Node(None,3,None)))),4,Some(Node(Some(Node(None,5,None)),6,Some(Node(None,7,None))))))
    val res2 = Some(Node(Some(Node(None,1,None)),2,None))
    assert(createBinTree(in1) ==  res1)
    assert(createBinTree(in2) ==  res2)
  }

  "4.4 Getting level order Nodes in a tree" should "return expected results" in {
    val in = Some(Node(Some(Node(Some(Node(None,1,None)),2,Some(Node(None,3,None)))),4,Some(Node(Some(Node(None,5,None)),6,Some(Node(None,7,None))))))
    assert(levelOrder(in) == List(List(6, 2), List(4), List(7, 5, 3, 1)))
  } 

  "4.6 find lca" should "return expected results" in {
    val in = Some(Node(Some(Node(Some(Node(None,1,None)),2,Some(Node(None,3,None)))),4,Some(Node(Some(Node(None,5,None)),6,Some(Node(None,7,None))))))
    assert(lca(in,Node(None,3,None),Node(None,5,None)) == in)
  }

  "8.1 nth Fibonacci number" should "return expected result" in {
    assert(fibonacci(13) == 144)
  }

  "8.3 / 8.4 generate possible subset of a set" should "return expected result" in {
    val in = List(1,2,3)
    assert(powerSet(in.toList) == List(List(), List(1), List(2), List(2, 1), List(3), List(3, 1), List(3, 2), List(3, 2, 1)))

     val in1 = "test".toList
    assert(powerSet(in1.toList) == List(List(), List('t'), List('e'), List('e', 't'), List('s'), List('s', 't'), List('s', 'e'), List('s', 'e', 't'), List('t'), List('t', 't'), List('t', 'e'), List('t', 'e', 't'), List('t', 's'), List('t', 's', 't'), List('t', 's', 'e'), List('t', 's', 'e', 't')))
  }

   "8.5 generate possible matching param" should "return expected result" in { 
    assert(matchParan(3) == List())
  }

  "8.7 calculate the number of ways of representing n cents" should "return expected result" in { 
      assert(makeChange(3,List(1,2,3)) == 1)
  }  

    "8.8 arranging queens safely in a chess board" should "return expected result" in {
      assert(arrangeQueens(8).length == 92)
    }

    "9.1 merge two array's without temp" should "return expected result" in {
      val in1 = Array.ofDim[Int](9)
      in1(0) = 1
      in1(1) = 5
      in1(2) = 7
      in1(3) = 9
      in1(4) = 10
      val in2 = Array(2,4,6,8)
      assert(mergeArray(in1,in2,5,4).deep == Array(1,2,4,5,6,7,8,9,10).deep)
    }

    "9.2 sort by anagram" should "return expected result" in {
      val in = List("stone age" ,"debit card", "stage one","bad credit")
      val re = List("debit card", "bad credit", "stone age", "stage one")
        assert(sortByAnagram(in) == re) 
    }

    "9.3 Modified Bin search" should "return expected results" in {
      val in = Array(15,16,19,20,25,1,3,4,5,7,10,14)
      assert(modifiedBinSearch(in,5,0,11) == Some(8))
      assert(modifiedBinSearch(in,9,0,11) == None)
    }

    "9.3 finding in a sorted matrix" should "return expected results" in {
      import scala.collection.{IndexedSeq => $}
      val in = $(
        $(4,7,9),
        $(9,15,21),
        $(11,17,23)
      )
      assert(findInMatrix(in,17,0,2,3) == true)
      assert(findInMatrix(in,10,0,2,3) == false)      
    }

    "19.1 swap two variables without temp" should "return explected result" in{
      assert(swapNoTemp(3,5)==(3,5))
    }


    "19.2 game of tic-tac-toe" should "return explected result" in{
      assert(ticTacToe()== 0)
    }

    "19.3 trailing zeros in n factorial" should "return explected result" in{
      assert(trailingZeros1(100,0,5) == 24)
    }

    "19.4 max of a number without comparision" should "return explected result" in{
      assert(getMaxNoComp(100,200) == 200)
    }

    "19.5 Find number  of hits and pseudo hits in a guessing game" should "return explected result" in{
      assert(estimateScore("RGGB","YRGB") == (2,1))
    }

    "19.6 Lergest sum in a sequence" should "return explected result" in{
      assert(largestSumInSeq(List(2, -8, 3, -2, 4, -10)) == 5)
    }

    "19.8 find the frequency of occurrences of any given word in a book." should "return explected result" in{
      val in = Array("test this test","nothing to test","nothing here","test is ok")
      assert(findWordCount("test",in) == 4)
    }

    "19.10 generating ran7 from ran5" should "return explected result" in{
      val r = ran7
      assert(r >= 0 &&  r<= 7 )
    }

    "19.11 all pairs in seq matching sum" should "return explected result" in{
      val i = List(4,5,6,3,8,1,2)
      val i2 = List(1, 4, 45, 6, 10, 8)
      assert(findAllPairs(i,7)==List((5,2),(6,1),(4,3)))
      assert(findAllPairs(i2,16)==List((6,10)))
    }
 }

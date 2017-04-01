package algos.tree 

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class BinaryTreeSpec extends FlatSpec  {

import algos.tree.BinaryTree._
	val n4 = Node(None,4,None)
	val n5 = Node(None,5,None)
	val n6 = Node(None,6,None)
	val n7 = Node(None,7,None)
	val n2 = Node(Some(n4),2,Some(n5))
	val n3 = Node(Some(n6),3,Some(n7))
	val n1 = Node(Some(n2),1,Some(n3))

	
	"preoder traversal of a binary tree" should "produce expected result" in {
		assert(preOrder(Some(n1)) == List(1,2,4,5,3,6,7))
	}

	"find depth of a binary tree" should "produce expected result" in {
		assert(depth(Some(n1)) == 3)
	}

	"finding LCA in a binary tree" should "produce expected result" in {
		assert(findLCA(Some(n1),n7,n6).get == n3)
	}
}


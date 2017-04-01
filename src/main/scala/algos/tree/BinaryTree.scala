package algos.tree 

import scala.Ordering.Implicits._

object BinaryTree {
	type Tree[A] = Option[Node[A]]

	case class Node[A](left:Tree[A],value:A,right:Tree[A]) {

		override def equals(that:Any):Boolean = that match {
			case otherNode:Node[A] => this.value == otherNode.value 
			case _ => false 
		}

		
	}

	def preOrder[A](root:Tree[A]):List[A] = root match {
		case None => Nil
		case Some(Node(l,v,r)) =>v::preOrder(l):::preOrder(r)
	}

	def depth[A](root:Tree[A]):Int = root match {
		case None => 0
		case Some(Node(l,v,r)) => 1 + math.max(depth(l),depth(r))
	}

	def findLCA[A](root:Tree[A],n1:Node[A],n2:Node[A]):Option[Node[A]] = root match {
		case None => None 
		case Some(n) if(n1 == n || n2 == n) => Some(n)
		case Some(Node(l,v,r)) =>  (findLCA(l,n1,n2) ,findLCA(r,n1,n2)) match {
			 case (None,None) => None
			 case (Some(_),Some(_)) => Some(Node(l,v,r))
			 case (None,Some(_)) => findLCA(r,n1,n2)
			 case (Some(_),None) => findLCA(l,n1,n2)
		}
	}
}


sealed trait BST[A] {
	val value:A
}

case class Node[A:Ordering](left:BST[A],override val value:A,	right:BST[A]) extends BST[A] {
	require(left.value <= value && value <= right.value)
	}

case class Leaf[A:Ordering](override val value:A) extends BST[A]

object BinarySearchTree {

}

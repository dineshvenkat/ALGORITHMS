package algos.CI_QUESTIONS

object Solution {
	
 /*
	1.1 Implement an algorithm to determine if a string has all 
	 unique characters. What if you can not use additional
	 data structures?

    Time - o(n), space o(n)
 */

 def uniqueChar(i:String):Boolean = {

   def loop(in:List[Char],dp:Array[Int]) :Boolean = in match {
   	case Nil => true
   	case x::xs => if(dp(x.toInt) == 1) false else {
   		dp(x.toInt) =1
   		loop(xs,dp)
   }
 }

   var a = Array.ofDim[Int](256)
   loop(i.toList,a)
 }

def uniqueCharBitwise(i:String):Boolean = {

   def loop(in:List[Char],mem:Int) :Boolean = in match {
      case Nil => true
      case x::xs =>  {
         val t = x - 'a'
         println(mem,t)
         if( (mem & (1 << t)) == 0 ) loop(xs,mem | (1 << t)) else false
      }
   }

   loop(i.toList,0)
 }

  /* 
   
   1.3 Design an algorithm and write code to remove the duplicate characters 
   in a string without using any additional buffer. NOTE: One or two 
   additional variables are fine. An extra copy of the array is not.
  */ 

  def removeDups(i:String) :String = {
   def loop(in:List[Char],out:List[Char],mem:Array[Int]) :String = in match {
      case Nil => out.reverse.mkString("")
      case x::xs => if(mem(x.toInt) == 1) loop(xs,out,mem) else {
         mem(x.toInt) =1
         loop(xs,x::out,mem)
   }
 }

   var a = Array.ofDim[Int](256)
   loop(i.toList,List[Char](),a)
  }

  /* 
   1.4 Write a method to decide if two strings are anagrams or not.
   */

   def checkAnagram(i:String,j:String):Boolean = i.sortWith(_ < _) == j.sortWith(_ < _)


  /*
   1.5 Write a method to replace all spaces in a string with ‘%20’. 
  */ 

  def replaceSpace(i:String):String = i.replace(" ","%20")

  /*
  Assume you have a method isSubstring which checks if one word is a 
  substring of another. Given two strings, s1 and s2, write code to check
 if s2 is a rotation of s1 using only one call to isSubstring (i.e., “waterbottle” 
 is a rotation of “erbottlewat”).
  */

  def rotateCheck(s1:String,s2:String) = (s1 + s1).indexOf(s2) != -1 

  /* 2.1 Write code to remove duplicates from an unsorted linked list. */
  def removeDupsInList[A](in:List[A]) = in.distinct

  /* 2.2 Implement an algorithm to find the nth to last element of a singly linked list. */
  def findNthElemInList[A](in:List[A],n:Int):Option[A] = {

    def loop(p:List[A],count:Int):Option[A] = p match {
      case Nil  => None
      case x::xs if(count <= 1) => Some(x)
      case x::xs  => loop(xs,count-1)
    }
    loop(in,n)
  }

  /* 2.3 Implement an algorithm to delete a node in the middle of a single 
  linked list, given only access to that node. */
  def deleteElemInList[A](in:List[A],d:A):List[A] = 
      in.foldRight(List[A]())((c,acc) => if(c == d) acc else c::acc)

  /*
  2.4 You have two numbers represented by a linked list, 
  where each node contains a single digit. The digits are stored 
   in reverse order, such that the 1’s digit is at the head of the list.
   Write a function that adds the two numbers and returns the sum as a linked list.
  */

  def addListNums(in1:List[Int],in2:List[Int]) : List[Int] = {
    def s1(a:Int,b:Int,c:Int) = ((a + b + c) % 10)
    def s2(a:Int,b:Int,c:Int) = ((a + b+ c) / 10)

   def loop(i1:List[Int],i2:List[Int],o:List[Int],c:Int):List[Int] = (i1,i2) match {
      case (Nil,Nil) => if(c == 0 ) o.reverse else (1::o).reverse
      case (Nil,y::ys) => loop(i1,ys,s1(0,y,c)::o,s2(0,y,c))
      case (x::xs,Nil) => loop(xs,i2,s1(0,x,c)::o,s2(0,x,c))
      case (x::xs,y::ys) => loop(xs,ys,s1(x,y,c)::o,s2(x,y,c))
   }

   loop(in1,in2,List[Int](),0)
  }

  
    /* 4.1 Implement a function to check if a tree is balanced. 
   For the purposes of this question, a balanced tree is
    defined to be a tree such that no two leaf nodes differ
     in distance from the root by more than one.
    */

    type Tree[A] = Option[Node[A]]
    case class Node[A](lt:Tree[A],v:A,rt:Tree[A]) {

      override def equals(that:Any) = that match {
        case that:Node[A] => this.lt == that.lt && this.v == that.v 
        case _ => false
      }
    }

    def isBalanced[A](re:Tree[A]):Boolean =  re match {
          case None => true 
          case Some(Node(None,_,None)) => true
          case Some(Node(Some(Node(l,_,r)),_,None)) => (l,r) match {
              case (None,None) => true
              case _ => false
              }
          case Some(Node(None,_,Some(Node(l,_,r)))) => (l,r) match {
              case (None,None) => true
              case _ => false
              }
           case Some(Node(l,_,r)) => isBalanced(l) && isBalanced(r)       
    }
  
   /* 4.3 Given a sorted (increasing order) array, write an algorithm 
    to create a binary tree with minimal height.
    */
  
  def createBinTree[A](in:List[A]):Tree[A] = in.size match {
        case 0 => None
        case 1 => Some(Node(None,in.head,None))
        case _ => {
          val mid = in.length /2 
          val (l,r) = in.splitAt(mid)
          Some(Node(createBinTree(l),r.head,createBinTree(r.tail)))
        }
  }

  /* 4.4 Given a binary search tree, design an algorithm which creates a
   linked list of all the nodes at each depth (i.e., if you have a tree with
    depth D, you’ll have D linked lists). */

  def levelOrder[A](r:Tree[A]):List[List[A]] = {
    import scala.collection.mutable.Map

    def loop(c:Tree[A],levelNum:Int=1,acc:Map[Int,List[A]]= Map[Int,List[A]]()):
      Map[Int,List[A]] = c match {
    case None => acc
    case Some(Node(l,v,r)) => { 
      if(acc.contains(levelNum))
        acc(levelNum)= v::acc(levelNum)
       else acc(levelNum) = List(v)
      loop(l,levelNum+1,acc)
      loop(r,levelNum+1,acc)
    }
  }
  loop(r).values.toList
  }


   /* 4.6 Design an algorithm and write code to find the first 
   common ancestor of two nodes in a binary tree. Avoid storing 
   additional nodes in a data structure. NOTE: This is not necessarily
    a binary search tree. */ 

  def lca[A](r:Tree[A],i1:Node[A],i2:Node[A]) : Option[Node[A]] = r match {
    case None => None
    case Some(n)  if(n.v == i1.v || n.v == i2.v) => Some(n)
    case Some(Node(l,v,r)) => (lca(l,i1,i2),lca(r,i1,i2)) match {
      case (None,None) => None
      case (Some(_),Some(_)) => Some(Node(l,v,r))
      case (Some(le),None) => lca(Some(le),i1,i2)
      case (None,Some(ri)) => lca(Some(ri),i1,i2)
    }
  }

  /* 8.1 Write a method to generate the nth Fibonacci number.  */ 
  import scala.annotation.tailrec
  def fibonacci(n:Int):Int = {

    @tailrec def loop(count:Int,c:Int,p:Int):Int = {
      if(count == 0) 0
      else if(count <=2 ) c
      else 
       loop(count-1,c+p,c)
    }
    loop(n,1,0)
  }

  /* 8.3/8.4  Write a method that returns all subsets of a set. */
  def powerSet[A](p:List[A]):List[List[A]] = {

    @tailrec
    def loop(c:List[A],acc:List[List[A]]):List[List[A]] = c match {
      case Nil => acc
      case x::xs => {
        println(x)
        loop(xs,acc:::acc.map(x::_))
      }
    }

    loop(p,List(List()))
  }

  /* 
    8.5 Implement an algorithm to print all valid (e.g., properly opened 
      and closed) combinations of n-pairs of parentheses.

EXAMPLE:
input: 3 (e.g., 3 pairs of parentheses)
output: ()()(), ()(()), (())(), ((()))
  */

   def matchParan(n:Int):List[String] = {
    val o = '('
    val c = ')'

    def loop(temp:String,oCount:Int,cCount:Int,acc:List[String]) :List[String] = 
        (oCount,cCount) match {
          case (x,y) if(x == n && y == n) => temp::acc
          case (x,y) if(x < n) => loop(temp + o,oCount +1, cCount,acc)
          
          case (x,y) if(y < n) =>   loop(temp + c,oCount, cCount + 1,acc)
      }

     loop("",0,0,List[String]())
      
    }

    /* 8.7 Given an infinite number of quarters (25 cents), 
     dimes (10 cents), nickels (5 cents) and pennies (1 cent),
     write code to calculate the number of ways of representing n cents.
    */
    def makeChange(s:Int,coins:List[Int]):Int = 0
    
    /* 8.8 Write an algorithm to print all ways of arranging
     eight queens on a chess board so that none of them share the
     same row, column or diagonal.
    */
   
   def arrangeQueens(n:Int):List[List[(Int,Int)]] = {

      def isSafe(queen:(Int,Int),queens:List[(Int,Int)]) = 
        queens forall (e => !isCheck(queen,e))

      def isCheck(c:(Int,Int),e:(Int,Int)) = c._1 == e._1 ||
        c._2 == e._2 || (c._1 - e._1).abs == (c._2 - e._2).abs 


      def loop(k:Int):List[List[(Int,Int)]] = if( k == 0 ) List(List()) else {
       
      for {
        queens <- loop(k-1)
       
        col <- 1 to n   
        queen = (k,col)
      
        if(isSafe(queen,queens))
      } yield queen::queens
      }
      loop(n)
   }

   /* 9.1 You are given two sorted arrays, A and B, and A has a
    large enough buffer at the end to hold B. Write a method to 
    merge B into A in sorted order.
    */
    
    import scala.Ordering.Implicits._
    def mergeArray[A:Ordering ](in1:Array[A],in2:Array[A],n:Int,m:Int):Array[A] = {
      var k = in1.size - 1
      var i = n-1
      var j = m -1

      while(i >= 0 && j >= 0 ){
        if(in1(i) > in2(j)) {
          in1(k) = in1(i)
          i = i-1
        } else {
          in1(k) = in2(j)
          j = j-1
        }
        k = k-1
      }

      while(j >=0 ) {
        in1(k) = in2(j)
        k = k-1
        j = j-1
    }
    in1
  }

  /* 9.2 Write a method to sort an array of strings
   so that all the anagrams are next to each other. */

  def sortByAnagram(i:List[String]) = i.sortWith( _.sorted < _.sorted )

  /* 9.3 Given a sorted array of n integers that has been rotated 
  an unknown number of times, give an O(log n) algorithm that finds
   an element in the array. You may assume that the array was originally 
   sorted in increasing order. */

  import scala.Ordering.Implicits._
  def modifiedBinSearch[T:Ordering](in:Array[T],x:T,l:Int,r:Int):Option[Int]  = {

    def binSearch(l1:Int,r1:Int):Option[Int] = {
       if(l1 > r1) None else {
       val m1 = (l1 + r1)/2
       if(in(m1) == x) Some(m1)
       else if(x > in(m1) )
        binSearch(m1+1,r1)
       else 
        binSearch(l1,m1-1)
      }
    }

    val m = (l + r) / 2 

    if(in(m) == x)
      Some(m)
    else if(in(l) < in(m)) { 
        if( x > in(m)){
          modifiedBinSearch(in,x,m+1,r)
        } else binSearch(l,m)
    } else if( x < in(m +1))
        modifiedBinSearch(in,x,l,m)
        else binSearch(m+1,r) 
  }

  /* 
  9.6 Given a matrix in which each row and each column is sorted, 
  write a method to find an element in it.
  */ 

  def findInMatrix[T:Ordering](in:IndexedSeq[IndexedSeq[T]],x:T,r:Int,c:Int,N:Int):Boolean ={
    if(r >= N || c <= 0 ) false
    else if(in(r)(c) == x) true
    else if(in(r)(c) > x )
    findInMatrix(in,x,r,c-1,N)
    else findInMatrix(in,x,r+1,c,N)
  }

  /* 19.1 Write a function to swap a number in place without temporary variables. */

  def swapNoTemp(a:Int,b:Int):(Int,Int) = {
    (a,b)
  }

  /* 19.2 Design an algorithm to figure out if someone has won in a game of tic-tac-toe. */
  def ticTacToe() = 1 

  /* 19.3 Write an algorithm which computes the number of trailing zeros 
   in n factorial 

    = floor(n/5) + floor(n/25) + floor(n/125) + ....
   */

    def trailingZeros1(factNum:Int,acc:Int,div:Int):Int = if(factNum < 0) acc
    else if (factNum / div <= 0) acc
     else  trailingZeros1(factNum,acc + factNum / div, div*5)

  /*
   19.4 Write a method which finds the maximum of two numbers.
    You should not use if-else or any other comparison operator.  

    max = a - k(a-b) where k is 1 if a-b is negative
  */
  
  def getMaxNoComp(a:Int,b:Int) = {
    val c = a - b
    val k = (c >> 31) & (0x1)
    a - k * c
  }
  
  /* 
    19.5 Find number  of hits and pseudo hits in a guessing game
  */

  def estimateScore(s:String,g:String):(Int,Int) = 
    (s.toList zip g.toList).foldLeft(0,0)((acc,c) => 
      if(c._1 == c._2)
        (acc._1 +1,acc._2)
       else if(s.contains(c._2))
      (acc._1,acc._2 +1) else acc)


  /*
  19.6 Given an integer between 0 and 999,999, print an English phrase that
   describes the integer (eg, “One Thousand, Two Hundred and Thirty Four”).
  */  

  /* 19.7 You are given an array of integers (both positive and negative).
   Find the continuous sequence with the largest sum. Return the sum. */

   def largestSumInSeq(in:List[Int]):Int = 
      in.scanLeft(0)(_ + _ max 0 ).max

  /* 19.8 Design a method to find the frequency of occurrences of
   any given word in a book.  */

   def findWordCount(s:String, book:Array[String]) = 
       book.map(line => {
         val t = line.split(" ").groupBy(i => i).filter(_._1 == s)
         if(t.isEmpty) 0 else (t.head._2).length
          }
        ).sum

  /*
    19.10 Write a method to generate a random number between 1 and 7,
     given a method that generates a random number between 1 and 5 
     (i.e., implement rand7() using rand5()).
  */     

  def ran7 = nextTemp % 7 + 1 

   def nextTemp:Int = {
    val r = 5 * (ran5 - 1) + ran5
    if (r > 21 )
      nextTemp
    else r
   }

  def ran5 =  scala.util.Random.nextInt(5)

  /* 
    19.11 Design an algorithm to find all pairs of integers within an array 
    which sum to a specified value.
  */

  def findAllPairs(in:List[Int],x:Int):List[(Int,Int)] = {
      var t = scala.collection.mutable.Map[Int,Int]()
      in.foldLeft(List[(Int,Int)]())((acc,c) => if(t.contains(x-c))
          (x-c,c)::acc
          else {
          t(c) = 1
          acc})
    }

  /*
    20.1 Add without using + operator 
  */  
  def add_no_arithm(a:Int,b:Int):Int = if(b == 0) a else {
    val sum = a ^ b //Add without carry
    val carry = (a & b) << 1 //Just carry with left shift
    add_no_arithm(sum,carry)
  }

  /* 20.3 Write a method to randomly generate a set of m integers 
  from an array of size n. Each element must have equal probability of being chosen */
  def buildRandomSet[T](in:Array[T],m:Int):Set[T] = {
      def getRan(start:Int,end:Int):Int = start + scala.util.Random.nextInt(end - start)
      
      def loop(p:Int,start:Int,end:Int,acc:Set[T]):Set[T] = if(p == 0) acc else {
        val cr = getRan(start,end)
        val toSet = in(cr)
        in(cr) = in(start)
        loop(p-1,start+1,end,acc + toSet)
      }
      loop(m,0,in.size-1,Set[T]())
  }

  /* 20.5 You have a large text file containing words.
   Given any two words, find the shortest distance
    (in terms of number of words) between them in the file. 
    Can you make the searching operation in O(1) time? What about
     the space complexity for your solution?

  */ 
  
 /* def minWordDistance(words:List[String],s1:String,s2:String):Option[Int] = {
    var counter:Int = 0
   val myHash = words.foldLeft(scala.collection.mutable.Map[String,List[Int]]())((acc,line) => {
    line.split(" ").reduceLeft(acc)((accword => {
         counter = counter + 1
        if(acc.contains(word)) {
          val t:List[Int] = acc(word)
          //acc + (word -> counter::t)
          acc + (word -> List(counter))
           }
          else 
           acc + (word -> List(counter))
      })
    })

  
   def findMinDistance(l1:List[Int],l2:List[Int],acc:(Int,Int)): (Int,Int) = {
      for{
        i <- l1
        j <- l2
        if((i-j).abs < (acc._1 - acc._2).abs)
      } yield(i,j)
    }

      findMinDistance(myHash(s1),myHash(s2),(0,0))
   } */
 }


/*
   3.1 Describe how you could use a single array to implement three stacks
  */
  class ThreeStackWithArray[A: Manifest](aSize:Int,default:A){
    

    private[this] var cArray = Array.fill(aSize)(default)
    case class StackPos(min:Int,max:Int,curr:Int)
    private[this] var s1 = StackPos(0,(aSize/3)-1,0)
    private[this] var s2 = StackPos((aSize/3),(2 * aSize/3) -1 ,(aSize/3))
    private[this] var s3 = StackPos((2 * aSize/3) ,aSize-1 ,(2 * aSize/3))

     def pushOnStack(in:A,s:StackPos) =  cArray(s.curr) = in
     def popFromStack(s:StackPos):A =  {
         val r = cArray(s.curr-1) 
         cArray(s.curr-1) = default
         r
      } 


    def pushTo(input:A,stackNum:Int) :Array[A] = stackNum match {
      case 1 => {
         pushOnStack(input,s1)
         s1=StackPos(s1.min,s1.max,s1.curr+1)
         cArray
      }
      case 2 => {
         pushOnStack(input,s2)
         s2=StackPos(s2.min,s2.max,s2.curr+1)
         cArray
      }
      case 3 => {
         pushOnStack(input,s3)
         s3=StackPos(s3.min,s3.max,s3.curr+1)
         cArray
      }
    }

    def popFrom(stackNum:Int) :(A,Array[A]) = stackNum match {
      case 1 => {
         val r = popFromStack(s1)
         s1=StackPos(s1.min,s1.max,s1.curr-1)
         (r,cArray)
      }
      case 2 => {
         val r =popFromStack(s2)
         s2=StackPos(s2.min,s2.max,s2.curr+1)
         (r,cArray)
      }
      case 3 => {
         val r = popFromStack(s3)
         s3=StackPos(s3.min,s3.max,s3.curr+1)
         (r,cArray)
      }
    }

  }

  /*
   How would you design a stack which, in addition to push and pop, 
   also has a function min which returns the minimum element?
    Push, pop and min should all operate in O(1) time.
  */
  class SpecialStack[A](val stack:List[(A,A)] = List[(A,A)]()) {
    
    def push(in:A)(implicit o:Ordering[A]):SpecialStack[A] = if(this.stack == Nil) {
               new SpecialStack((in,in)::this.stack)
         } else {
            if(o.lt(in , this.stack.head._1))
               new SpecialStack((in,in)::this.stack)
            else
               new SpecialStack((this.stack.head._1,in)::this.stack)
         }

    def pop :(A,SpecialStack[A]) = 
      (this.stack.head._2,new SpecialStack(this.stack.tail))
    
    def min :A = this.stack.head._1

    override def toString = stack.mkString(",")
  }

   /*
     3.3 Imagine a (literal) stack of plates. 
     If the stack gets too high, it might topple. 
     Therefore, in real life, we would likely start a new stack when the 
     previous stack exceeds some threshold. Implement a data structure SetOfStacks
      that mimics this. SetOfStacks should be composed of several stacks, and should 
      create a new stack once the previous one exceeds capacity. 
      SetOfStacks.push() and SetOfStacks.pop() should behave identically to a single 
      stack (that is, pop() should return the same values as it would if there were 
      just a single stack).
   */

  class SetOfStacks[A](val n:Int, val stacks:List[List[A]] = List[List[A]]()) {

   def push(v:A):SetOfStacks[A] = if(!stacks.isEmpty){
         if(stacks.head.length >= n)
            new SetOfStacks[A](n,List(v)::stacks)
         else
            new SetOfStacks[A](n,(v::stacks.head)::stacks.tail)  
         } else 
           new SetOfStacks[A](n,List(v)::stacks) 

   def pop:(A,SetOfStacks[A]) =  if(stacks.head.length == 1)
            (stacks.head.head,new SetOfStacks[A](n,stacks.tail))
         else
            (stacks.head.head,new SetOfStacks[A](n,stacks.head.tail::stacks.tail))
    
    override def toString = stacks.toString
            
    } 


 class Graph(num:Int,isDirected:Boolean= false) {

    private[this] val adjList = Array.fill(num)(scala.collection.mutable.Map[Int,Double]())

    import Graph.EndPoints

     private[this] implicit class Edge(p:EndPoints) {
      val (u, v) = p
      assume(hasVertices(u, v))
    }

    def vertices = adjList.indices

    def hasVertices(vs: Int*) = vs forall vertices.contains


     def neighbors(n:Int) = adjList(n).keySet

     def update(p:EndPoints,w:Double) {
      adjList(p._1)(p._2) = w 

      if(!isDirected)
        adjList(p._2)(p._1) = w 
     }

     def edges = for {
        u <- adjList.indices
        v <- adjList(u).keySet
     } yield (u -> v)

     def apply(u:Int)(v:Int):Double = this(u -> v)

     def apply(p:EndPoints):Double = if(p.u == p.v) 0.0 else adjList(p.u)(p.v)

 } 

 object Graph {
    private[Graph] type EndPoints = (Int,Int)


    def isConnected(g:Graph,u:Int,v:Int):Boolean = {
    
    def loop(stack:List[Int],s:Set[Int]):Boolean = if (!stack.isEmpty && !s(stack.head)) { 
       if(stack.head == v) true 
       else 
      loop(g.neighbors(stack.head).foldLeft(stack.tail)((acc,c)=>   c::acc),s + stack.head)
      } else if(!stack.isEmpty && s(stack.head)) loop(stack.tail,s)
      else false
      
    loop(List[Int](u),Set[Int]())
    
  }
 }  


   

   
  
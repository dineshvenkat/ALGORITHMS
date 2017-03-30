package algos.dp

object DP {


  def LargestCommonSeq[A](a:List[A], b:List[A]): List[A] = {

    def loop(ap:List[A],bp:List[A],c:List[A]):List[A] = (ap,bp) match {
      case (_,Nil) => c.reverse
      case (Nil,_) => c.reverse
      case (x::xs,y::ys) if(x==y) => loop(xs,ys,x::c)
      case (x::xs,y::ys) => {
        val la = loop(xs,bp,c)
        val lb = loop(ap,ys,c)
        //println(s"${la} - ${lb}")
        if(la.length > lb.length) la else lb
      }
    }
    loop(a,b,List[A]())
  }


  def editDist[A](f:List[A],s:List[A],
                  in:A=>Int,
                  de:A=> Int,
                  sw: (A,A) => Int,
                  dp:scala.collection.mutable.Map[(List[A],List[A]),Int]= scala.collection.mutable.Map.empty[(List[A], List[A]), Int]) :Int = {


    def cacheGet(first:List[A],second:List[A]):Int =
      dp.getOrElseUpdate((first,second),editDist(first,second,in,de,sw,dp))


    (f,s) match {
      case (Nil,Nil) => 0
      case (Nil,x) => x.map(in).sum
      case (x,Nil) => x.map(de).sum
      case (x::xs,y::ys) => List( cacheGet(xs,ys)+sw(x,y),
        cacheGet(xs,s)+in(y),
        cacheGet(f,ys)+de(x)
      ).min
    }
  }


  def knapSack(W:Int,n:Int,v:Array[Int],w:Array[Int],dp:scala.collection.mutable.Map[Int,Int]) : Int  = {

    def getFromCache(value:Int,num:Int) =
      dp.getOrElseUpdate(value,knapSack(value,num,v,w,dp))

    if(W <= 0 || n == w.length) 0
    else  {

      if(w(n) > W)
        getFromCache(W,n+1)
      else {
        val i = v(n) + getFromCache(W-w(n),n+1)
        val e = getFromCache(W,n+1)
        math.max(i,e)
      }
    }

  }

  def coinChange(c:List[Int],a:Int,dp:scala.collection.mutable.Map[Int,List[Int]]):List[Int] = {

    def getFromCache(p:Int):List[Int] = dp.getOrElseUpdate(p,coinChange(c,p,dp))

    if(a <= 0) List()
    else {
      val (x,_) = (c.filter(c1 => c1 <= a)).map(c2 => (c2,getFromCache(a-c2).length)).minBy(_._2)
      x::getFromCache(a-x)
    }
  }


  def maxSumInSeq(in:Seq[Int]):Int = in.scanLeft(0)(_ + _ max 0).max


  def longestIncreasingSeq[B](input:List[B])(implicit o:Ordering[B]): List[B] = {

    def init(i:Int, l:List[B],m:Map[Int,List[B]]):Map[Int,List[B]] = {
      if(l.isEmpty)
        m
      else
        init(i+1,l.tail,m+(i->List(l.head)))
    }

    def loop(count:Int,p:List[B],dp:Map[Int,List[B]]):List[B] = p match {
      case Nil => dp.maxBy(_._2.length)._2.reverse
      case x::xs => {
        val f = dp.filter(c => c._1 < count && o.lt(c._2.head , x))
        if(f.isEmpty)
          loop(count + 1, xs, dp)
        else {
          val (_,ll:List[B]) = f.maxBy(_._2.length)
          val test = x::ll
          loop(count + 1, xs, dp + (count -> test))
        }
      }
    }

    if(input.isEmpty)
      Nil
    else
      loop(1,input,init(0,input,Map[Int,List[B]]()))
  }

  def minimumJump(in:List[Int]) : Int = {

    def loop(p:List[(Int,Int)],dp:Map[Int , (Int,Int,Int)]) :Int = p match {
      case Nil => dp.max._2._2
      case x::xs => {
        val candidates = dp.filter { case (k:Int,v:(Int,Int,Int)) =>  if(k + v._1 >= x._2) true else false }
        if(candidates.isEmpty) loop(xs,dp+ (x._2+1 -> (x._1,1,0)))
        val (ck,cv) = candidates.minBy{ case(k1:Int,v:(Int,Int,Int)) => v._2 }
       // println(s"${x._2+1} - ${x._1},${cv._2+1},${ck}")
        loop(xs,dp+ (x._2+1 -> (x._1,cv._2+1,ck)))
    }}

      //val in1 = in.zipWithIndex.foldLeft(List[(Int,Int)]())((acc,c) => (c._1,c._2 + 1)::acc ).reverse
    loop(in.zipWithIndex,Map[Int,(Int,Int,Int)](0 -> (0,0,-1)))
  }


def numEncoding(in:List[Char],dp:scala.collection.mutable.Map[String,Int]):Int = {

  def getFromCache(i:String) = 
    dp.getOrElseUpdate(i,{ 
      //println(s"$i - not in cache")
      numEncoding(i.toList,dp) })

//println(in)
in match {
 case Nil => 1
 case x:: Nil => 1
 case x:: xs if(x == 0) => getFromCache(xs.mkString) 
 case x:: xs => if(s"${x}${xs.head}".toInt <= 26) {
    getFromCache(xs.mkString)  + getFromCache(xs.tail.mkString) }
    else {
     getFromCache(xs.mkString) 
    }
  }
 }


   def stackBox(in:List[(Int,Int,Int)]) :Int = {

    def loop(p:List[(Int,Int,Int)],dp:Map[(Int,Int,Int),Int]) : Int = p match {
      case Nil => dp.maxBy(_._2)._2
      case x::xs => { 
           val v1 = dp.filter { case(k:(Int,Int,Int),v:Int) => (k._1 > x._1 && k._2 > x._2) }
            if(v1.isEmpty) {
                //println(s"${x} -> ${x._3}")
                loop(xs,dp + (x -> x._3)) }else { val v2 = v1.maxBy(_._2)._2
                //  println(s"${x} -> ${v2 + x._3}")
           loop(xs,dp + (x -> (v2 + x._3)))
         }
    }
  }


   def sortByBase(toSort:List[(Int,Int,Int)]): List[(Int,Int,Int)] = 
        toSort.sortWith((x,y) => (x._1 * x._2) > (y._1 * y._2))    

    def generateCombo(t:(Int,Int,Int)):List[(Int,Int,Int)] = 
        t.productIterator.toList.permutations.toList.map(x =>  x match {
             case List(a:Int, b:Int, c:Int, _*) => (a, b, c)})
  
        loop(sortByBase(in.flatMap(generateCombo)),Map[(Int,Int,Int),Int]())
   }

   def subSetSum(s:List[Int],sum:Int,dp:scala.collection.mutable.Map[(List[Int],Int),Boolean]):Boolean = { 

    def getFromCache(ns:List[Int],s1:Int) = dp.getOrElseUpdate((ns,s1),subSetSum(ns,s1,dp))

    s match {
     case Nil  => if(sum > 0) false else true
     case x:: xs => getFromCache(xs,sum) || getFromCache(xs,sum-x)
     }
   }
  
}

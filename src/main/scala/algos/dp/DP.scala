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

  def main(args:Array[String]) {

    val a = "ABCDGH".toList
    val b = "AEDFHR".toList
    println(LargestCommonSeq(a,b).mkString(" "))
  }
}

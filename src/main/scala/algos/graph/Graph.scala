package algos.graph

class Graph(val numVertices:Int,val isDirected:Boolean = true) {
	
	import Graph.EndPoints

	 private[this] implicit class Edge(points: EndPoints) {
    val (u, v) = points
    assume(hasVertices(u, v))
  }  

	private[this] val adjList = Array.fill(numVertices)(scala.collection.mutable.Map[Int,Double]() withDefaultValue Double.PositiveInfinity)

	def apply(p:EndPoints):Double = if(p.u == p.v)	0.0 else adjList(p.u)(p.v)

	def apply(u: Int)(v: Int): Double = this(u->v)

	def has(p:EndPoints) : Boolean = adjList(p.u) contains (p.v)

	def hasVertices(vs: Int*) = vs forall vertices.contains

	def vertices = adjList.indices 

	def edges = for {
		u <- vertices
		v <- neighbors(u)
	  } yield u -> v
	

	def neighbors(u:Int) = adjList(u).keySet

	def update(p:EndPoints,w:Double) = { 
		adjList(p.u)(p.v) = w
		if(!isDirected)
		  adjList(p.v)(p.u) = w
	}

	def -=(p:EndPoints) =  {
		adjList(p.u) -= p.v
		if(isDirected)
			adjList(p.v) -= p.u
	}

	 def adjacencyMatrix = 
	   Array.tabulate(numVertices, numVertices){(u, v) => this(u->v)}
}

object Graph {

	import scala.collection.mutable

	private[Graph] type EndPoints = (Int, Int)

	def dfs(g:Graph,v:Int):Set[Int] = {
		
		def loop(stack:List[Int],s:Set[Int]):Set[Int] = if (!stack.isEmpty && !s(stack.head)) 
			loop(g.neighbors(stack.head).foldLeft(stack.tail)((acc,c)=>   c::acc),s + stack.head)
		  else if(!stack.isEmpty && s(stack.head)) loop(stack.tail,s)
		  else s
		  
		loop(List[Int](v),Set[Int]())
		
	}
  	import scala.collection.immutable.Queue
	def bfs(g:Graph,v:Int):Set[Int] = {
		
		def loop(q:Queue[Int],s:Set[Int]):Set[Int] = if (!q.isEmpty && !s(q.head)) 
			loop(g.neighbors(q.head).foldLeft(q.tail)((acc,c)=>  acc :+ c),s + q.head)
		  else if(!q.isEmpty && s(q.head)) loop(q.tail,s)
		  else s

		loop(Queue[Int](v),Set[Int]())
		
	}

	def dijkstra(g:Graph,s:Int,d:Int) = new GraphNavigator[Int]{ 
		assume(g hasVertices (s, d))
		override def distance(from:Int,to:Int) = g(from -> to)
		override def neighbors(c:Int) = g.neighbors(c)
	} navigate(s,_ == d)

	def primsMST(g:Graph) = g.vertices.toList match { 
		case Nil => scala.collection.mutable.Set[Int]()
		case v::vs => 
			val (seen,unseen,mst) = (scala.collection.mutable.Set(v),scala.collection.mutable.Set(vs: _*),scala.collection.mutable.Set[EndPoints]())
			while(!unseen.isEmpty){
			  val r = for { 
			  		 x <- seen
					 y <- unseen 		
					} yield(x,y)
			  //println(s"seen=${seen},unseen=${unseen},${r}")
			  val (a,b) = r minBy(i => g(i))	
			    unseen -= b
			  seen += b
			  println(s"seen=${seen},unseen=${unseen},a=${a}-${b}")
			  mst += a->b 
			}
			mst.toSet
		}
}
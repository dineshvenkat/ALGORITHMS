
package algos.graph
case class NavResult[Node] (cost:Double,path:Seq[Node])

trait GraphNavigator[Node] {
 import Implicits._	

	def navigate(start:Node, isGoal:Node => Boolean):Option[NavResult[Node]] = {
		val score = scala.collection.mutable.Map(start -> 0d) withDefaultValue Double.PositiveInfinity 
		val priority = Ordering by {n:Node => score(n) }
		val queue = scala.collection.mutable.TreeSet(start)(priority)
		val parent = scala.collection.mutable.Map.empty[Node, Node]
		val visited = scala.collection.mutable.Set.empty[Node]

		def reScore(current: Node)(n: Node) = {
     		 score(n) = score(current) + distance(current, n)
    
    	}

		while(!queue.isEmpty) {
			 val current = queue.head
			 queue -= current 
			 if (isGoal(current)) {
        val trace =  scala.collection.mutable.ArrayBuffer.empty[Node]
        var (v, cost) = (current, 0d)
        while (parent contains v) {
          cost += distance(parent(v), v)
          v +=: trace
          v = parent(v)
        }
        return Some(NavResult(cost, start +: trace.toSeq))
      }

			/* neighbors(current) filterNot visited.contains foreach { n => 
			 	if(score(n) > score(current) + distance(current,n)) {
			 		queue updatePriority (n, reScore(current))
			 		parent(n) = current
			 	}
			 }*/

			 neighbors(current) filterNot visited.contains foreach { n =>
        if(score(n) >= score(current) + distance(current, n)) {
          
          queue -= n
          
          //update score
          score(n) = score(current) + distance(current, n)
          queue += n
         parent(n) = current
        }
      }
			  visited += current
		}

		None
	}

	def distance(from: Node, to: Node) = 1d

	def neighbors(n:Node): Iterable[Node]

	
}

object Implicits {
 implicit class Updateable[A](queue: scala.collection.mutable.TreeSet[A]) {
    /**
     * Remove and return the smallest value from a TreeSet queue
     * O(log n)
     */
    def removeFirst = {
      val head = queue.head
      queue -= head
      head
    }

    /**
     * Hack to update priority of a node by deleting and re-adding
     * O (log n)
     *
     * @param node node whose priority is being updated
     * @param update a function given a node updates its priority (is called for given node)
     */
    def updatePriority(node: A, update: A => Unit) {
      queue -= node
      update(node)
      queue += node
    }
  }
}

package algos.dp

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

class GraphSpec extends FlatSpec  {

  import algos.graph.Graph._
  import algos.graph._

  val testUndi:Graph = new Graph(4,false)

  testUndi.update((0,1),0)
  testUndi.update((0,2),0)
  testUndi.update((2,0),0)
  testUndi.update((2,3),0)
  testUndi.update((3,3),0)
  testUndi.update((1,2),0)

 "DFS traveral on the Graph" should "give expected result" in {
  	  assert(dfs(testUndi,2) == Set(2,0,1,3))
  } 

  "BFS traveral on the Graph" should "give expected result" in {
  	  assert(bfs(testUndi,2) == Set(2,0,3,1))
  }

  val testG1:Graph = new Graph(9,false)
  testG1.update((0,1),4)
  testG1.update((0,7),8)
  testG1.update((1,7),11)
  testG1.update((1,2),8)
  testG1.update((2,8),2)
  testG1.update((7,8),7)
  testG1.update((8,6),6)
  testG1.update((7,6),1)
  testG1.update((2,5),4)
  testG1.update((2,3),7)
  testG1.update((3,5),14)
  testG1.update((3,4),9)
  testG1.update((5,4),10)
  testG1.update((6,5),2)


  "Dijkstra's traveral on the Graph" should "give expected result" in {
  	  val NavResult(c,_) = dijkstra(testG1,0,4).get
  	  assert( c == 21)
  }

  "Prims's MST on the Graph" should "give expected result" in {
  	  val res = Set((0,1),(0,7),(7,6),(6,5),(5,2),(2,8),(2,3),(3,4))
  	  assert( primsMST(testG1) == res)
  }
}
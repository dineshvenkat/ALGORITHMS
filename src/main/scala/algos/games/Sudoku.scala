package algos.games

case class Sudoku(board:IndexedSeq[IndexedSeq[Int]]) {
    private[this] val n = board.length
    val t = Math.sqrt(n)
    private[this] val s = Math.sqrt(n).toInt.ensuring(i => i*i == n, "Size must be a perfect square")
    require(t * t == n)

    def solve: Option[Sudoku] = solve(0)

    private[Sudoku] def solve(cell:Int):Option[Sudoku] = (cell%n,cell/n) match {
        case (_,`n`) => if(isSolution) Some(this) else None
        case (r,c) if(board(r)(c) > 0) => solve(cell + 1)
        case (r,c) => 
           def used(i:Int) = Seq(board(r)(i),board(i)(c),board(s*(r/s) + i/s)(s*(c/s) + i%s))
           def guess(i:Int) = (this(r,c)=i).solve(cell + 1)

           val v = (1 to n) diff board.indices.flatMap(x => used(x))
           v firstDefined guess
    }


    def update(r:Int,c:Int,v:Int) = Sudoku(board.updated(r,board(r).updated(c,v)))

    def isSolution : Boolean = {
        val expected = (1 to n ).toSet
          board.indices forall { i => 
            (board.indices map(c => board(i)(c))).toSet == expected &&
            (board.indices map(c => board(c)(i))).toSet == expected &&
            (board.indices map {j => board(s*(i/s) + j%s)(s*(i%s) + j/s)}).toSet == expected
        }
    }


    override def toString = 
        board grouped s map {_ map {_ grouped s map {_ mkString " "} mkString " | "} mkString "\n"} mkString s"\n${"-" * 11 mkString " "}\n"

        implicit class TraversableExtension[A](t: Traversable[A]) {
    def firstDefined[B](f: A => Option[B]): Option[B] = t collectFirst Function.unlift(f)
  }
}




/*
    Using IndexedSeq to represent a board 
    
    private within a instance, within  class

    Ability to use update method in a call 
*/
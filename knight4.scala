// Part 4 about finding a single tour on "mutilated" chessboards
//==============================================================

object M4d { 

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala or knight3.scala                  !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)
type Path = List[Pos]

def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((i, j))}%4.0f ")
    }
    println()
  } 
}

// ADD YOUR CODE BELOW
//======================

// (10)
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if ((x._1 >= dim || x._2 >= dim || x._1 < 0 || x._2 < 0) || path.contains(x)) false else true
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
val moves = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)) //stores the list for all possible moves
(for (i <- 0 to moves.size-1 if(is_legal(dim,path,(x._1+moves(i)._1,x._2+moves(i)._2)))) yield (x._1+moves(i)._1,x._2+moves(i)._2)).toList
}
def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val moves = legal_moves(dim,path,x)
    moves.sortBy(pos => legal_moves(dim,path,pos).size)
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case Nil => None
  case x :: tail => {
    val proc = f(x)
    if (proc != None) proc else first(tail,f)
  }
}

def one_tour_pred(dim: Int, path: Path, n: Int, pred: Pos => Boolean): Option[Path] = {
  if(path.size == n) Some(path)
  else{
    val moves = ordered_moves(dim,path,path.head)
    val valid_moves = (for(i <- 0 to moves.size-1 if(pred(moves(i)))) yield moves(i)).toList
    first(valid_moves,p=>one_tour_pred(dim,p::path,n,pred))
  } 
}

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================
import scala.annotation.tailrec

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
def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if (path.size == dim * dim) Some(path)
    else first(ordered_moves(dim,path,path.head),p=>first_tour_heuristics(dim,p::path))
}
//(9) 
def trecursive_funct(dim:Int,paths:List[Path]): Option[Path] = paths match {
    case Nil => None
    case path::rest =>{
        if(path.size == dim * dim) Some(path)
        else{
          val p = ordered_moves(dim,path,path.head)
          trecursive_funct(dim,p.map(mp => mp::path) ::: paths.tail)
        }
    }
}

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    trecursive_funct(dim,path::List())
}
    
}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

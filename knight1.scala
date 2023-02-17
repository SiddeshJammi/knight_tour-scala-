// Main Part 4 about finding Knight's tours
//==========================================


object M4a {

// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end of the file are of any help.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================



//(1) 
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if ((x._1 >= dim || x._2 >= dim || x._1 < 0 || x._2 < 0) || path.contains(x)) false else true

}


//(2) 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
val moves = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)) //stores the list for all possible moves
(for (i <- 0 to moves.size-1 if(is_legal(dim,path,(x._1+moves(i)._1,x._2+moves(i)._2)))) yield (x._1+moves(i)._1,x._2+moves(i)._2)).toList
}


//some testcases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


// (3) 
def count_tours(dim: Int, path: Path) : Int = { 
  val moves = legal_moves(dim,path,path.head)
  if(moves == Nil){ //checks if there are no more moves that can be made without repeating a square
    if(path.size == dim * dim) 1 else 0 //a knights tour is only possible iff each square is visited which for e.g. 8 x 8 is 64 (8^2)
    
  }
  else{
      moves.map(c => count_tours(dim, c :: path)).sum 
  }
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
  val moves = legal_moves(dim,path,path.head)
  if(moves == Nil) {
    if(path.size == dim * dim) List(path) else Nil
  }
  else{
    moves.flatMap(c => enum_tours(dim, c :: path))
}
}

// (4) 
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case Nil => None
  case x :: tail => {
    val proc = f(x)
    if (proc != None) proc else first(tail,f)
  }
}


// testcases
//
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) 
def first_tour(dim: Int, path: Path) : Option[Path] = {
  if(path.size == dim * dim) Some(path) else first(legal_moves(dim,path,path.head),p => first_tour(dim,p::path))
}
 


/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//  
//     time_needed(count_tours(dim, List((0, 0))))
//
// in order to print out the time that is needed for 
// running count_tours


// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}


*/

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

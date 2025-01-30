package graph

import scala.annotation.tailrec

object GraphTypes {
  // Adjacency List/Map
  type AdjacencyMap[T] = Map[T, Set[T]]
  
  val sampleAdjMap: AdjacencyMap[String] = Map(
    "A" -> Set("B","C"),
    "B" -> Set("C", "D"),
    "C" -> Set(""),
    "D" -> Set("A","C"))

  // Adjacency Matrix
  type AdjacencyMatrix[T] = Seq[Seq[T]]
  // Node Set + Edge Set
  
  def main(args: Array[String]): Unit = {
    //val matrix: AdjacencyMatrix[Int] = Vector(Vector(1,0,0),Vector(0,0,1),Vector(0,0,0))
    //val seq1 = matrix(1)(1)
    println(isPath(sampleAdjMap, "C","D"))
  }
  
  
  def outDegree[T](graph: AdjacencyMap[T], node: T): Int = {
    if(graph.contains(node)) graph(node).size
    else 0
  }
  
  def inDegree[T](graph: AdjacencyMap[T], node: T): Int = {
    graph.values.count(_.contains(node))
  }
  
  def isPath[T](graph: AdjacencyMap[T], start: T, end: T): Boolean = {
    @tailrec
    def helper(todo: List[T], done: Set[T]): Boolean = {
      if(todo.isEmpty) false
      else if(todo.head == end) true
      else if(done.contains(todo.head)) helper(todo.tail, done)
      else if(graph.contains(todo.head)) helper(graph(todo.head) ++: todo.tail, done + todo.head)
      else helper(todo.tail, done)
    }
    helper(List(start),Set.empty[T])
  }
}

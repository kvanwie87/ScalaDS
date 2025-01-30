package graph

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

object GraphProblems {
  // Adjacency List/Map - key values of node to neighbors
  // Adjacency Matrix - 2d matrix indicating connections
  // Node Set + Edge Set - sets for nodes and sets for edges between nodes
  // Graph Node Objects - object representing node and has connections inside of it
  // directed and undirected
  // weighted edges vs unweighted
  // cycles
  // paths and connectivity, disconnected and disjoint
  // traversals - dfs and bfs
  // degree, in-degree, out-degree
  def main(args: Array[String]): Unit = {
    testLongestCycle()
  }

  /*
  Find if Path Exists in Graph
  https://leetcode.com/problems/find-if-path-exists-in-graph/description/?envType=problem-list-v2&envId=graph
  There is a bi-directional graph with n vertices, where each vertex is labeled from 0 to n - 1 (inclusive).
  The edges in the graph are represented as a 2D integer array edges, where each edges[i] = [ui, vi] denotes a bi-directional edge between vertex ui and vertex vi.
  Every vertex pair is connected by at most one edge, and no vertex has an edge to itself.
  You want to determine if there is a valid path that exists from vertex source to vertex destination.
  Given edges and the integers n, source, and destination, return true if there is a valid path from source to destination, or false otherwise.
   */
  def validPath(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    val adjacencyMap = scala.collection.mutable.Map[Int, scala.collection.mutable.ListBuffer[Int]]()
    edges.foreach(edge => {
      if (adjacencyMap.contains(edge(0))) adjacencyMap(edge(0)).addOne(edge(1)) else adjacencyMap.put(edge(0), ListBuffer(edge(1)))
      if (adjacencyMap.contains(edge(1))) adjacencyMap(edge(1)).addOne(edge(0)) else adjacencyMap.put(edge(1), ListBuffer(edge(0)))
    })
    val toVisit = scala.collection.mutable.Queue[Int]()
    val visited = scala.collection.mutable.Set[Int]()
    toVisit += source

    while(toVisit.nonEmpty) {
      val curr = toVisit.dequeue()
      if(curr == destination) return true
      if(!visited.contains(curr)) {
        val neighbors = adjacencyMap.getOrElse(curr, ListBuffer.empty[Int])
        neighbors.foreach(toVisit += _)
        visited += curr
      }
    }
    false
  }

  def validPath_v2(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    val adjacencyMap = edges.foldLeft(Map.empty[Int,List[Int]])((result, edge) => result + (edge(0) -> (edge(1) +: result.getOrElse(edge(0), List.empty[Int]))) + (edge(1) -> (edge(0) +: result.getOrElse(edge(1), List.empty[Int]))))
    @tailrec
    def bfs(toVisit: Queue[Int], visited: Set[Int]): Boolean = {
      if(toVisit.isEmpty) false
      else if(toVisit.head == destination) true
      else if(visited.contains(toVisit.head)) bfs(toVisit.tail, visited)
      else bfs(toVisit.tail ++ adjacencyMap.getOrElse(toVisit.head, List.empty[Int]).filter(!visited.contains(_)), visited + toVisit.head)
    }
    bfs(Queue(source), Set.empty[Int])
  }

  /*
  Rotting Oranges
  https://leetcode.com/problems/rotting-oranges/description/
  You are given an m x n grid where each cell can have one of three values:
  0 representing an empty cell,
  1 representing a fresh orange, or
  2 representing a rotten orange.
  Every minute, any fresh orange that is 4-directionally adjacent to a rotten orange becomes rotten.
  Return the minimum number of minutes that must elapse until no cell has a fresh orange. If this is impossible, return -1.
   */
  def testOrangesRotting(): Unit = {
    //[[2,1,1],[1,1,0],[0,1,1]]

    val input = Array.ofDim[Int](3,3)
    input(0) = Array(2,1,1)
    input(1) = Array(1,1,0)
    input(2) = Array(0,1,1)
   // assert(orangesRotting(input) == 4)
    val input2 = Array.ofDim[Int](1, 2)
    input2(0) = Array(0,1)

    assert(orangesRotting(input2) == -1)
  }


  def orangesRotting(grid: Array[Array[Int]]): Int = {
    def getAdjacentIndices(index: (Int,Int,Int)): List[(Int, Int, Int)] = {
      List((index._1 + 1,index._2, index._3 + 1),(index._1 - 1,index._2, index._3 + 1),(index._1,index._2 + 1, index._3 + 1),(index._1,index._2 - 1, index._3 + 1))
        .filter(index => index._1 > -1 && index._2 > -1 && index._1 < grid.length && index._2 < grid(0).length && grid(index._1)(index._2) == 1)
    }
    var freshFruitRemaining = 0
    var minutes = 0
    val toVisit = scala.collection.mutable.Queue[(Int, Int, Int)]()
    grid.indices.foreach(i => {
      grid(i).indices.foreach(j => {
        if(grid(i)(j) == 2) toVisit.enqueue((i,j,0))
        else if(grid(i)(j) == 1) freshFruitRemaining += 1
      })
    })
    while(toVisit.nonEmpty) {
      val curr = toVisit.dequeue()
      minutes = curr._3
      val adjacentFruits = getAdjacentIndices(curr)
      adjacentFruits.foreach(index => grid(index._1)(index._2) = 2)
      freshFruitRemaining -= adjacentFruits.length
      toVisit.enqueueAll(adjacentFruits)
    }
    if(freshFruitRemaining == 0) minutes else -1
  }

  def orangesRotting_v2(grid: Array[Array[Int]]): Int = {
    val fruits = grid.indices.flatMap(i => grid(i).indices.map(j => (i,j,grid(i)(j)))).filter(_._3 != 0)
    val rotten = fruits.filter(_._3 == 2).map(fruit => (fruit._1, fruit._2, 0))
    val freshCount = fruits.count(_._3 == 1)

    def getAdjacentFreshFruit(index: (Int, Int, Int)): List[(Int, Int, Int)] = {
      List((index._1 + 1, index._2, index._3 + 1), (index._1 - 1, index._2, index._3 + 1), (index._1, index._2 + 1, index._3 + 1), (index._1, index._2 - 1, index._3 + 1))
        .filter(index => index._1 > -1 && index._2 > -1 && index._1 < grid.length && index._2 < grid(0).length && grid(index._1)(index._2) == 1)
    }
    @tailrec
    def helper(toVisit: Queue[(Int, Int, Int)], freshRemaining: Int, minute: Int): Int = {
      if(toVisit.isEmpty) if(freshRemaining == 0) minute else -1
      else if(freshRemaining == 0) minute
      else {
        val curr = toVisit.head
        val adjacentFreshFruits = getAdjacentFreshFruit(curr)
        adjacentFreshFruits.foreach(fruit => grid(fruit._1)(fruit._2) = 2)
        helper(toVisit.tail ++ adjacentFreshFruits, freshRemaining - adjacentFreshFruits.length, curr._3 + 1)
      }
    }
    helper(Queue.from(rotten), freshCount, 0)
  }

  /*
  Word Ladder
  https://leetcode.com/problems/word-ladder/description/
   */
  def testLadderLength(): Unit = {
    // hit -> hot -> dot -> dog -> cog
    println(ladderLength_v2("hit", "cog", List("hot","dot","dog","lot","log","cog")))
  }

  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {
    val wordSet = wordList.toSet
    val charRange = 'a' to 'z'
    def adJWords(currWord:String): Seq[String] = {
      (0 until currWord.length).flatMap(i => {
        val before = currWord.take(i)
        val after = currWord.drop(i + 1)
        val words = charRange.filterNot(_ == currWord.charAt(i)).map(before + _ + after)
        val validWords = words.filter(wordSet.contains)
        validWords
      })
    }
    val visited = scala.collection.mutable.Set[String]()
    val toVisit = scala.collection.mutable.Queue[(String, Int)]()
    toVisit.enqueue((beginWord, 1))

    while(toVisit.nonEmpty) {
      val (currWord, dist) = toVisit.dequeue()
      visited.add(currWord)
      if(currWord.equals(endWord)) return dist
      toVisit.enqueueAll(adJWords(currWord).filter(!visited.contains(_)).map(x=>(x,dist + 1)))
    }
    0
  }

  def ladderLength_v2(beginWord: String, endWord: String, wordList: List[String]): Int = {
    val wordSet = wordList.toSet
    val charRange = 'a' to 'z'

    def adJWords(currWord: String): Seq[String] = {
      (0 until currWord.length).flatMap(i => {
        val before = currWord.take(i)
        val after = currWord.drop(i + 1)
        val words = charRange.filterNot(_ == currWord.charAt(i)).map(before + _ + after)
        val validWords = words.filter(wordSet.contains)
        validWords
      })
    }
    def helper(toVisit: Queue[(String, Int)], visited: Set[String]): Int = {
      if(toVisit.isEmpty) 0
      else if(toVisit.head._1.equals(endWord)) toVisit.head._2
      else helper(toVisit.tail ++ adJWords(toVisit.head._1).filter(!visited.contains(_)).map(x => (x, toVisit.head._2 + 1)), visited + toVisit.head._1)
    }
    helper(Queue((beginWord,1)), Set.empty[String])
  }

  /*
  Is Graph Bipartite?
  https://leetcode.com/problems/is-graph-bipartite/description/
  There is an undirected graph with n nodes, where each node is numbered between 0 and n - 1.
  You are given a 2D array graph, where graph[u] is an array of nodes that node u is adjacent to.
  More formally, for each v in graph[u], there is an undirected edge between node u and node v.
  The graph has the following properties:
    There are no self-edges (graph[u] does not contain u).
    There are no parallel edges (graph[u] does not contain duplicate values).
    If v is in graph[u], then u is in graph[v] (the graph is undirected).
    The graph may not be connected, meaning there may be two nodes u and v such that there is no path between them.

  A graph is bipartite if the nodes can be partitioned into two independent sets A and B such that every edge in the graph
  connects a node in set A and a node in set B.

  Return true if and only if it is bipartite.
   */
  def testIsBipartite(): Unit = {
    val input1 = Array.ofDim[Int](4,0)
    input1(0) = Array(1,3)
    input1(1) = Array(0,2)
    input1(2) = Array(1,3)
    input1(3) = Array(0,2)
    println(isBipartite_v2(input1))
    val input2 = Array.ofDim[Int](4, 0)
    input2(0) = Array(1,2,3)
    input2(1) = Array(0,2)
    input2(2) = Array(0,1,3)
    input2(3) = Array(0,2)
    println(isBipartite_v2(input2))
  }
  def isBipartite(graph: Array[Array[Int]]): Boolean = {
    val partitions = Array.ofDim[Int](graph.length)
    def bfs(idx: Int): Boolean = {
      val toVisit = scala.collection.mutable.Queue[(Int, Int)]((idx, 1))
      partitions(idx) = 1
      while(toVisit.nonEmpty) {
        val (curr, partition) = toVisit.dequeue()
        val neighbors = graph(curr)
        if(neighbors.exists(neighbor => partitions(neighbor) == partition)) return false
        val neighborsToProcess = neighbors.filter(partitions(_) == 0).map((_, partition * -1))
        toVisit.enqueueAll(neighborsToProcess)
        neighborsToProcess.foreach(neighbor => partitions(neighbor._1) = neighbor._2)
      }
      true
    }

    graph.indices.foldLeft(true)((cont, i) => {
      if(cont)
        if(partitions(i) == 0 && !bfs(i)) false
        else true
      else
        false
    })
  }
  def isBipartite_v2(graph: Array[Array[Int]]): Boolean = {
    @tailrec
    def startBFSs(partitions: Map[Int,Int], startPoints: Seq[Int]): Boolean = {
      if(startPoints.isEmpty) true
      else {
        if(!partitions.contains(startPoints.head)) {
          val (result, newPartitions) = bfs(partitions, Queue((startPoints.head, 1)))
          if (!result) false
          else startBFSs(newPartitions, startPoints.tail)
        } else {
          startBFSs(partitions, startPoints.tail)
        }
      }
    }
    @tailrec
    def bfs(partitions: Map[Int,Int], toVisit: Queue[(Int, Int)]): (Boolean, Map[Int, Int]) = {
      if(toVisit.isEmpty) (true, partitions)
      else {
        val (curr, partition) = toVisit.head
        val neighbors = graph(curr)
        if (neighbors.exists(neighbor => partitions.getOrElse(neighbor, 0) == partition)) (false, partitions)
        else {
          val neighborsToProcess = neighbors.filter(partitions.getOrElse(_, 0) == 0).map((_, partition * -1))
          bfs(partitions ++ neighborsToProcess, toVisit.tail ++ neighborsToProcess)
        }

      }
    }
    startBFSs(Map.empty[Int,Int], graph.indices)
  }

  /*
  Longest Cycle in a Graph
  https://leetcode.com/problems/longest-cycle-in-a-graph/description/
  You are given a directed graph of n nodes numbered from 0 to n - 1, where each node has at most one outgoing edge.
  The graph is represented with a given 0-indexed array edges of size n,
  indicating that there is a directed edge from node i to node edges[i].
  If there is no outgoing edge from node i, then edges[i] == -1.
  Return the length of the longest cycle in the graph. If no cycle exists, return -1.

  A cycle is a path that starts and ends at the same node.
   */
  def testLongestCycle(): Unit = {
    println(s"Result: ${longestCycle(Array(3,3,4,2,3))}, Expected: 3")
    println(s"Result: ${longestCycle(Array(2,-1,3,1))}, Expected: -1")
    println(s"Result: ${longestCycle(Array(5,8,-1,5,-1,6,1,6,6,5))}, Expected: 3")
  }
  def longestCycle(edges: Array[Int]): Int = {
    val visited = Array.ofDim[Boolean](edges.length)
    def bfs(start: Int): Int = {
      val dist = scala.collection.mutable.Map[Int, Int]()
      dist.put(start, 1)
      var curr = start
      while(curr != -1 && !visited(curr)) {
        visited(curr) = true
        val next = edges(curr)
        if(next != -1 && dist.contains(next)) {
          return dist(curr) - dist(next) + 1
        }
        dist.put(next, dist(curr) + 1)
        curr = next
      }
      -1
    }
    edges.indices.map(idx => bfs(edges(idx))).max
  }
}

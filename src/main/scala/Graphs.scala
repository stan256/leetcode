object Graphs extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var neighbors: List[Node] = List()
  }



  // 547. Number of Provinces
  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    var map = isConnected.indices.map(_ -> List.empty[Int]).toMap
    val seen = collection.mutable.ArraySeq.fill(isConnected.length){false}
    var answer = 0

    for (i <- isConnected.indices) {
      var list = map(i)
      for (j <- isConnected(i).indices) {
        if (isConnected(i)(j) == 1 && i != j)
          list = list :+ j
      }
      map = map + (i -> list)
    }


    def dfs(x: Int): Unit = {
      val list = map(x)
      for (l <- list) {
        if (!seen(l)) {
          seen(l) = true
          dfs(l)
        }
      }
    }


    for (x <- isConnected.indices) {
      if (!seen(x)) {
        answer += 1
        seen(x) = true
        dfs(x)
      }
    }

    answer
  }

  // 200. Number of Islands
  def numIslands(grid: Array[Array[Char]]): Int = {
    val directions = List((-1, 0), (0, -1), (1, 0), (0, 1))
    val seen = Array.fill(grid.length)(Array.fill(grid(0).length)(false))
    var answer = 0

    def check(x: Int, y: Int, isNew: Boolean): Unit = {
      if (x < 0 ||
        y < 0 ||
        x >= grid.length ||
        y >= grid(0).length ||
        seen(x)(y)
      ) return
      seen(x)(y) = true
      if (grid(x)(y) == '1') {
        if (isNew) answer += 1
        for (d <- directions) check(x + d._1, y + d._2, isNew = false)
      }
    }
    for (x <- grid.indices) {
      for (y <- grid(0).indices) {
        check(x, y, isNew = true)
      }
    }
    answer
  }

  // 1971. Find if Path Exists in Graph
  def validPath(n: Int,
                edges: Array[Array[Int]],
                source: Int,
                destination: Int): Boolean = {

    if (source == destination) return true
    if (edges.isEmpty) return false

    val map = collection.mutable.Map.empty[Int, List[Int]]
    for (edge <- edges){
      val a = edge(0)
      val b = edge(1)

      if (map.contains(a)) map.put(a, b :: map(a))
      else map.put(a, List(b))

      if (map.contains(b)) map.put(b, a :: map(b))
      else map.put(b, List(a))
    }

    val queue = collection.mutable.Queue.empty[Int]
    queue.prependAll(map(source))
    val memo = collection.mutable.Set.empty[Int]
    memo.add(source)

    while (queue.nonEmpty) {
      val value = queue.removeLast()

      if (value == destination) return true
      else if (!memo.contains(value)) {
        memo.add(value)
        queue.prependAll(map(value))
      }
    }

    false
  }

  val directions = Array((1, 0), (0, 1), (-1, 0), (0, -1))
  def getAmountOfPaths(grid: Array[Array[Char]], robotLocation: Array[Int],  maxSteps: Int): Int = {
    val memo = grid.clone()

    var result = 0

    def search(step: Int, coordinate: (Int, Int)): Unit = {
      if (step > maxSteps ||
        coordinate._1 < 0 ||
        coordinate._2 < 0 ||
        coordinate._1 >= grid.length ||
        coordinate._2 >= grid(0).length ||
        memo(coordinate._1)(coordinate._2) == 'V' ||
        memo(coordinate._1)(coordinate._2) == 'X') return

      if (grid(coordinate._1)(coordinate._2) == 'C') {
        result += 1
      } else {
        memo(coordinate._1)(coordinate._2) = 'V'
        for (xy <- directions) {
          search(step + 1, (coordinate._1 + xy._1, coordinate._2 + xy._2))
        }
        memo(coordinate._1)(coordinate._2) = '0'
      }
    }


    search(0, (robotLocation(0), robotLocation(1)))

    result
  }

  // 133. Clone Graph
  def cloneGraph(graph: Node): Node = {
    if (graph == null) return null

    val map = collection.mutable.HashMap.empty[Node, Node]

    def updateAndReturnGraph(node: Node): Node = {
      if (!map.contains(node)) {
        val newNode = new Node(node.value)
        map.put(node, newNode)
      } else return map(node)
      map(node).neighbors = node.neighbors.map(updateAndReturnGraph)
      map(node)
    }

    updateAndReturnGraph(graph)
  }

  // 207. Course Schedule
  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    var arr = Array.ofDim[Int](numCourses)
    val map = Array.fill[List[Int]](numCourses) {List()}
    val queue = collection.mutable.Queue.empty[Int]
    for (p <- prerequisites) {
      arr(p(0)) += 1
      map(p(1)) = p(0) :: map(p(1))
    }
    for (i <- arr.zipWithIndex) if (i._1 == 0) queue.enqueue(i._2)
    var visited = 0
    while (queue.nonEmpty) {
      visited += 1
      val index = queue.dequeue()
      val dependants = map(index)
      for (d <- dependants) {
        arr(d) -= 1
        if (arr(d) == 0) queue.enqueue(d)
      }
    }
    visited == numCourses
  }


  // 210. Course Schedule II
  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    val arr = Array.ofDim[Int](numCourses)
    val dependants = Array.fill[List[Int]](numCourses) {List()}
    val queue = collection.mutable.Queue.empty[Int]

    for (p <- prerequisites) {
      dependants(p(1)) = p(0) :: dependants(p(1))
      arr(p(0)) += 1
    }
    for (a <- arr.zipWithIndex) if(a._1 == 0) queue.enqueue(a._2)

    val lb = collection.mutable.ListBuffer.empty[Int]
    var counter = 0
    while (queue.nonEmpty) {
      counter += 1
      val index = queue.dequeue()
      lb.addOne(index)
      val deps = dependants(index)
      for (d <- deps) {
        arr(d) -= 1
        if (arr(d) == 0) queue.enqueue(d)
      }
    }
    if (counter == numCourses) lb.toArray else Array()
  }


  // 994. Rotting Oranges
  def orangesRotting(grid: Array[Array[Int]]): Int = {
    val queue = collection.mutable.Queue.empty[Array[Int]]
    var totalFresh = 0
    for (i <- grid.indices) {
      for (j <- grid.head.indices) {
        if (grid(i)(j) == 1) totalFresh += 1
        else if (grid(i)(j) == 2) queue.enqueue(Array(i, j))
      }
    }
    if (queue.isEmpty && totalFresh == 0) return 0

    var counter = 0
    while (queue.nonEmpty) {
      counter += 1
      val size = queue.size

      for (i <- 0 until size) {
        val arr = queue.dequeue()
        Array((0, 1), (0, -1), (1, 0), (-1, 0))
          .map(t => (t._1 + arr(0)) -> (t._2 + arr(1)))
          .filter(t => t._1 >= 0 && t._1 < grid.length && t._2 >= 0 && t._2 < grid.head.length)
          .filter(t => grid(t._1)(t._2) == 1)
          .foreach(t => {
            queue.enqueue(Array(t._1, t._2))
            grid(t._1)(t._2) = 2
            totalFresh -= 1
          })
      }
    }

    if (totalFresh == 0) counter - 1 else -1
  }

  // 399. Evaluate Division
  def calcEquation(equations: List[List[String]],
                   values: Array[Double],
                   queries: List[List[String]]): Array[Double] = {

    import collection.mutable

    val map = mutable.Map.empty[String, mutable.Map[String, Double]]
    for ((eq, i) <- equations.zipWithIndex) {
      map.getOrElseUpdate(eq(0), mutable.Map.empty[String, Double]).put(eq(1), values(i))
      map.getOrElseUpdate(eq(1), mutable.Map.empty[String, Double]).put(eq(0), 1d / values(i))
    }

    var results = List[Double]()

    for (query <- queries) {
      val queue = collection.mutable.Queue.empty[(String, Double)]
      val seen = collection.mutable.HashSet.empty[String]
      val submap = map.getOrElse(query(0), collection.mutable.Map.empty)
      submap.foreach(queue.enqueue)
      seen.addAll(submap.keys)
      var result: Double = -1

      var found = false
      while (queue.nonEmpty && !found) {
        val value = queue.dequeue()

        if (value._1 == query(1)) {
          found = true
          if (result < 0) result = value._2
          else result *= value._2
        } else {
          map(value._1).foreach((k, v) => {
            if (!seen.contains(k)) {
              queue.enqueue((k, v * value._2))
              seen.addOne(k)
            }
          })
        }
      }

      results = results :+ result
    }

    results.toArray
  }
}

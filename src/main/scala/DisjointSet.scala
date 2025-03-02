object DisjointSet {
  class DisjointFastFind(size: Int) {
    val root: Array[Int] = (0 until size).toArray

    def find(x: Int): Int = root(x)

    def union(x: Int, y: Int): Unit = {
      val findx = find(x)
      val findy = find(y)
      if (findx != findy) {
        for (i <- root.indices) {
          if (root(i) == findy) {
            root(i) = findx
          }
        }
      }
    }

    def connected(x: Int, y: Int): Boolean = find(x) == find(y)
  }

  class DisjointFastUnion(size: Int) {
    val root = (0 until size).toArray

    def find(x: Int): Int = {
      var res = x

      while (res != root(res)) {
        res = root(res)
      }

      res
    }

    def union(x: Int, y: Int): Unit = {
      val findx = find(x)
      val findy = find(y)
      if (findx != findy) {
        root(findy) = findx
      }
      print("")
    }

    def connected(x: Int, y: Int): Boolean = find(x) == find(y)
  }

  class DisjointUnionByRankWithAmortizedConstantFind(size: Int) {
    val root = (0 until size).toArray
    val ranks = Array.fill(size)(1)

    def find(x: Int): Int = {
      if (x == root(x)) {
        return x
      }
      root(x) = find(root(x))
      root(x)
    }

    def union(x: Int, y: Int): Unit = {
      val rootX = find(x)
      val rootY = find(y)
      if (rootX != rootY) {
        if (ranks(rootX) > ranks(rootY)) {
          root(rootY) = rootX
        } else if (ranks(rootX) < ranks(rootY)) {
          root(rootX) = rootY
        } else {
          root(rootY) = rootX
          ranks(rootX) += 1
        }
      }
    }

    def connected(x: Int, y: Int): Boolean = find(x) == find(y)
  }

  val uf = new DisjointUnionByRankWithAmortizedConstantFind(10)
  uf.union(0, 1)
  uf.union(0, 2)
  uf.union(0, 3)
  uf.union(0, 4)
  uf.union(5, 6)
  uf.union(5, 7)
  uf.union(5, 8)
  uf.union(5, 9)
  uf.union(8, 3)

  uf.find(3)
  println()
}

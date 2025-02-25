object Trie extends App {
  // 208. Implement Trie (Prefix Tree)
  class Trie() {
    case class Node(var isPresent: Boolean = false, val children: Array[Node] = Array.fill[Node](26)(null))

    val topLevel = Node()

    def insert(word: String): Unit = {
      var currentLevel = topLevel
      for (c <- word) {
        if (currentLevel.children(c - 'a') == null) currentLevel.children(c - 'a') = Node()
        currentLevel = currentLevel.children(c - 'a')
      }
      currentLevel.isPresent = true
    }

    def search(word: String): Boolean = {
      var currentLevel = topLevel
      for (c <- word) {
        if (currentLevel != null) currentLevel = currentLevel.children(c - 'a')
      }
      currentLevel != null && currentLevel.isPresent
    }

    def startsWith(prefix: String): Boolean = {
      var currentLevel = topLevel
      for (c <- prefix) {
        if (currentLevel != null) currentLevel = currentLevel.children(c - 'a')
      }
      currentLevel != null
    }
  }

}

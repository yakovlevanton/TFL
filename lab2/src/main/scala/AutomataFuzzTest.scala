import scala.util.Random

object AutomataFuzzTest {
  val dfaTransitions: Map[(Int, Char), Int] = Map(
    // state 1
    (1, 'a') -> 2,
    (1, 'b') -> 1,

    // state 2
    (2, 'a') -> 2,
    (2, 'b') -> 3,

    // state 3
    (3, 'a') -> 4,
    (3, 'b') -> 5,

    // state 4
    (4, 'a') -> 6,
    (4, 'b') -> 7,

    // state 5
    (5, 'a') -> 6,
    (5, 'b') -> 8,

    // state 6
    (6, 'a') -> 9,
    (6, 'b') -> 10,

    // state 7
    (7, 'a') -> 11,
    (7, 'b') -> 12,

    // state 8
    (8, 'a') -> 9,
    (8, 'b') -> 13,

    // state 9
    (9, 'a') -> 2,
    (9, 'b') -> 3,

    // state 10
    (10, 'a') -> 4,
    (10, 'b') -> 12,

    // state 11
    (11, 'a') -> 6,
    (11, 'b') -> 7,

    // state 12
    (12, 'a') -> 6,
    (12, 'b') -> 8,

    // state 13
    (13, 'a') -> 2,
    (13, 'b') -> 13
  )

  def dfaAccepts(word: String): Boolean = {
    val accepting = Set(6, 7, 8, 9, 10, 11, 12, 13)

    val finalState = word.foldLeft(1) { (state, ch) =>
      dfaTransitions.getOrElse((state, ch), -1)
    }

    accepting.contains(finalState)
  }


  val nfaTransitions: Map[Int, Map[Char, Set[Int]]] = Map(
    1 -> Map('a' -> Set(1, 2), 'b' -> Set(1)),
    2 -> Map('a' -> Set(), 'b' -> Set(3)),
    3 -> Map('a' -> Set(4), 'b' -> Set(4)),
    4 -> Map('a' -> Set(5), 'b' -> Set(5)),
    5 -> Map('a' -> Set(6), 'b' -> Set(7)),
    6 -> Map('a' -> Set(), 'b' -> Set()),
    7 -> Map('a' -> Set(), 'b' -> Set(7))
  )

  def nfaAccepts(word: String): Boolean = {
    var states = Set(1)
    val accepting = Set(5, 6, 7)

    for (ch <- word) {
      if (!Set('a', 'b').contains(ch)) return false
      states = states.flatMap(s => nfaTransitions.getOrElse(s, Map()).getOrElse(ch, Set()))
      if (states.isEmpty) return false
    }

    states.exists(accepting.contains)
  }

  val subDfaTransitions: Map[(Int, Char), Int] = Map(
    (1, 'a') -> 2,
    (1, 'b') -> 1,
    (2, 'a') -> 2,
    (2, 'b') -> 3,
    (3, 'a') -> 4,
    (3, 'b') -> 4,
    (4, 'a') -> 5,
    (4, 'b') -> 5,
    (5, 'a') -> 5,
    (5, 'b') -> 5
  )

  def subDfaAccepts(word: String): Boolean = {
    val subDfaAccepting: Set[Int] = Set(5)
    val finalState = word.foldLeft(1) { (state, ch) =>
      subDfaTransitions.getOrElse((state, ch), -1)
    }
    subDfaAccepting.contains(finalState)
  }

  val regex = "^(a|b)*ab(a|b)(a|b)(a|b*)$".r
  def regexAccepts(word: String): Boolean =
    regex.matches(word)

  def afaAccepts(word: String): Boolean =
    subDfaAccepts(word) && dfaAccepts(word)

  def randomWords(n: Int, maxLen: Int, seed: Int = 42): Seq[String] = {
    val rnd = new Random(seed)
    (0 until n).map { _ =>
      val len = rnd.nextInt(maxLen + 1)
      (0 until len).map(_ => if (rnd.nextBoolean()) 'a' else 'b').mkString
    }
  }

  // --------------------- Fuzz test ------------------------

  def fuzzTest(numTests: Int = 1000, maxLen: Int = 40): Unit = {
    println("Fuzz tests for DFA, NFA, AFA and regex equivalence")

    val tests = randomWords(numTests, maxLen)

    var ok = true
    for ((word, i) <- tests.zipWithIndex) {
      val d = dfaAccepts(word)
      val n = nfaAccepts(word)
      val r = regexAccepts(word)
      val a = afaAccepts(word)
      if (!(d == n && n == r && r == a)) {
        ok = false
        println(s"\nNon-equivalence for word '$word'")
        println(s"   DFA:   $d")
        println(s"   NFA:   $n")
        println(s"   Regex: $r")
        println(s"   AFA:   $a")
      }
    }

    if (ok)
      println(s"\nAll $numTests passed successfully!")
    else
      println("\nFound incompatibility.")
  }


  def main(args: Array[String]): Unit = {
    fuzzTest()
  }
}

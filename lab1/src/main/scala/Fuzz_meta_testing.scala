import scala.util.Random
import scala.collection.mutable

object Fuzz_meta_testing {
  private val Alphabet: Vector[Char] = Vector('a', 'b', 'c', 's')
  private val OriginalRules: Vector[(String, String)] = Vector(
    "as"   -> "sa",
    "babs" -> "absa",
    "babb" -> "aabsa",
    "cs"   -> "abab",
    "abaa" -> "baba"
  )
  private val ExtendedRules: Vector[(String, String)] = OriginalRules ++ Vector(
    "aabsaabs"      -> "baabsaa",
    "abababa"       -> "aabsaaba",
    "bbaabsaa"      -> "aabsaabb",
    "bababababa"    -> "aabsaabbaba",
    "bbaabsaaabb"   -> "aabsbaabsaaa"
  )
  private def replaceOneRandomOccurrence(word: String, lhs: String, rhs: String): String = {
    val indices = mutable.ArrayBuffer[Int]()
    var start = 0
    while (start <= word.length - lhs.length) {
      val idx = word.indexOf(lhs, start)
      if (idx == -1) start = word.length + 1
      else {
        indices += idx
        start = idx + 1
      }
    }

    if (indices.isEmpty) word
    else {
      val randomPos = indices(Random.nextInt(indices.length))
      word.substring(0, randomPos) + rhs + word.substring(randomPos + lhs.length)
    }
  }
  private def generateRewriteChain(
                                    rules: Vector[(String, String)],
                                    minLen: Int = 15,
                                    maxLen: Int = 30,
                                    maxOps: Int = 20,
                                    maxAttempts: Int = 1000
                                  ): (String, String, Vector[String]) = {

    for (_ <- 1 to maxAttempts) {
      val wordLength = minLen + Random.nextInt(maxLen - minLen + 1)
      val initialWord = (1 to wordLength).map(_ => Alphabet(Random.nextInt(Alphabet.length))).mkString

      var current = initialWord
      val chain = mutable.ArrayBuffer(current)

      val numOps = 1 + Random.nextInt(maxOps)

      var step = 0
      while (step < numOps) {
        val applicableRules = rules.filter { case (lhs, _) => current.contains(lhs) }
        if (applicableRules.isEmpty) step = numOps
        else {
          val (lhs, rhs) = applicableRules(Random.nextInt(applicableRules.length))
          current = replaceOneRandomOccurrence(current, lhs, rhs)
          chain += current
          step += 1
        }
      }

      if (initialWord != current) {
        return (initialWord, current, chain.toVector)
      }
    }

    val fallbackLen = minLen + Random.nextInt(maxLen - minLen + 1)
    val fallbackWord = (1 to fallbackLen).map(_ => Alphabet(Random.nextInt(Alphabet.length))).mkString
    (fallbackWord, fallbackWord, Vector(fallbackWord))
  }
  
  private def findAllSuccessors(word: String, rules: Vector[(String, String)]): Vector[String] = {
    val results = mutable.ArrayBuffer[String]()
    for ((lhs, rhs) <- rules) {
      var start = 0
      while (start <= word.length - lhs.length) {
        val idx = word.indexOf(lhs, start)
        if (idx == -1) start = word.length + 1
        else {
          val next = word.substring(0, idx) + rhs + word.substring(idx + lhs.length)
          if (next != word) results += next
          start = idx + 1
        }
      }
    }
    results.toVector
  }
  
  private def findNormalForms(
                                  word: String,
                                  rules: Vector[(String, String)],
                                  maxDepth: Int = 1000
                                ): Set[String] = {
    val visited = mutable.Set[String]()
    val queue = mutable.Queue[(String, Int)]((word, 0))
    val normalForms = mutable.Set[String]()

    while (queue.nonEmpty) {
      val (current, depth) = queue.dequeue()
      if (depth <= maxDepth && !visited.contains(current)) {
        visited += current
        val successors = findAllSuccessors(current, rules)
        if (successors.isEmpty) {
          normalForms += current
        } else {
          successors.foreach { s =>
            if (!visited.contains(s)) queue.enqueue((s, depth + 1))
          }
        }
      }
    }
    normalForms.toSet
  }
  
  private def areEquivalent(
                             word1: String,
                             word2: String,
                             rules: Vector[(String, String)]
                           ): Boolean = {
    val nf1 = findNormalForms(word1, rules)
    val nf2 = findNormalForms(word2, rules)
    nf1.exists(nf2.contains)
  }

  private val Invariants: Vector[String => Int] = Vector(
    s => s.count(_ == 'a') + s.count(_ == 'b') + 4 * s.count(_ == 'c'),
    s => (s.count(_ == 'a') + s.count(_ == 'b')) % 2,
    s => if (s.contains('a') || s.contains('c')) 1 else 0,
    s => if (s.contains('b') || s.contains('s')) 1 else 0
  )

  private def checkInvariantsHold(rules: Vector[(String, String)]): Boolean = {
    val (_, _, chain) = generateRewriteChain(rules)
    for (i <- 1 until chain.length) {
      val prev = chain(i - 1)
      val curr = chain(i)
      if (Invariants.exists(inv => inv(prev) != inv(curr))) {
        return false
      }
    }
    true
  }

  def runMetaTesting(
                      rules: Vector[(String, String)],
                      numTests: Int
                    ): (Int, Int) = {
    var passed = 0
    for (_ <- 1 to numTests) {
      if (checkInvariantsHold(rules)) passed += 1
    }
    (passed, numTests)
  }

  def runFuzzTesting(
                      sourceRules: Vector[(String, String)],
                      targetRules: Vector[(String, String)],
                      numTests: Int
                    ): (Int, Int) = {
    var passed = 0
    for (i <- 1 to numTests) {
      val (w, transformedW, _) = generateRewriteChain(sourceRules)
      if (w == transformedW) {
        passed += 1
      } else if (areEquivalent(w, transformedW, targetRules)) {
        passed += 1
      } else {
        println(s"❌ Failed on test #$i: '$w' → '$transformedW' not equivalent under extended rules")
      }
    }
    (passed, numTests)
  }

  def main(args: Array[String]): Unit = {
    val numFuzzTests = 5000
    val numMetaTests = 5000
    println("=== Fuzz-testing ===")
    val (fuzzPassed, fuzzTotal) = runFuzzTesting(OriginalRules, ExtendedRules, numFuzzTests)
    println(s"Passed: $fuzzPassed / $fuzzTotal")

    println("\n=== Meta-testing ===")
    val (metaPassed, metaTotal) = runMetaTesting(ExtendedRules, numMetaTests)
    println(s"Passed: $metaPassed / $metaTotal")

    if (fuzzPassed == fuzzTotal && metaPassed == metaTotal) {
      println("\n🎉 All tests passed!")
    } else {
      println("\n⚠️ Some tests failed.")
    }
  }
}
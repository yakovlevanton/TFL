import scala.collection.mutable
import scala.util.Random

object BenchmarkParsers {

  private def matchLit(s: String, i: Int, lit: String): List[Int] =
    if (s.startsWith(lit, i)) List(i + lit.length) else Nil

  private def matchChar(s: String, i: Int, ch: Char): List[Int] =
    if (i < s.length && s.charAt(i) == ch) List(i + 1) else Nil

  // Grammar:
  // S -> B R
  // B -> "bab" | "bbbbbb" | ε
  // R -> "a" S "b" R | ε

  // 1) Naive parser (oracle)
  object NaiveRecursiveDescent {
    def parseS(input: String, i: Int): List[Int] = {
      val afterB = parseB(input, i)
      afterB.flatMap(j => parseR(input, j))
    }

    def parseB(input: String, i: Int): List[Int] = {
      val bab = matchLit(input, i, "bab")
      val b6  = matchLit(input, i, "bbbbbb")
      val eps = List(i)
      bab ++ b6 ++ eps
    }

    def parseR(input: String, i: Int): List[Int] = {
      val eps = List(i)

      val aThen = matchChar(input, i, 'a')
      val aS    = aThen.flatMap(j => parseS(input, j))
      val aSb   = aS.flatMap(j => matchChar(input, j, 'b'))
      val aSbR  = aSb.flatMap(j => parseR(input, j))

      eps ++ aSbR
    }

    def accepts(input: String): Boolean = {
      val ends = parseS(input, 0)
      ends.contains(input.length)
    }
  }

  // 2) O(n^3) parser
  final class OptimizedParser {
    private var input: String = ""
    private var n: Int = 0

    // 0 = unknown, 1 = visiting, 2 = done
    private var stS: Array[Array[Byte]] = _
    private var stR: Array[Array[Byte]] = _
    private var valS: Array[Array[Boolean]] = _
    private var valR: Array[Array[Boolean]] = _

    def accepts(s: String): Boolean = {
      // quick filter: suffix-balance check (b never less than a in any suffix)
      var bal = 0
      var i = s.length - 1
      while (i >= 0) {
        val ch = s.charAt(i)
        if (ch == 'b') bal += 1
        else if (ch == 'a') bal -= 1
        else return false
        if (bal < 0) return false
        i -= 1
      }

      input = s
      n = s.length
      stS = Array.fill(n + 1, n + 1)(0.toByte)
      stR = Array.fill(n + 1, n + 1)(0.toByte)
      valS = Array.fill(n + 1, n + 1)(false)
      valR = Array.fill(n + 1, n + 1)(false)
      canS(0, n)
    }

    // S -> B R; B -> "bab" | "bbbbbb" | ε
    private def canS(i: Int, j: Int): Boolean = {
      if (i < 0 || j < 0 || i > n || j > n || i > j) return false
      stS(i)(j) match {
        case 2 => return valS(i)(j)
        case 1 => return false
        case _ =>
      }
      stS(i)(j) = 1

      var ok = false

      // B -> ε
      if (canR(i, j)) ok = true

      // B -> "bab"
      if (!ok && matchLitAt(i, "bab")) {
        val k = i + 3
        if (k <= j && canR(k, j)) ok = true
      }

      // B -> "bbbbbb"
      if (!ok && matchLitAt(i, "bbbbbb")) {
        val k = i + 6
        if (k <= j && canR(k, j)) ok = true
      }

      valS(i)(j) = ok
      stS(i)(j) = 2
      ok
    }

    // R -> ε | a S b R
    private def canR(i: Int, j: Int): Boolean = {
      if (i < 0 || j < 0 || i > n || j > n || i > j) return false
      stR(i)(j) match {
        case 2 => return valR(i)(j)
        case 1 => return false
        case _ =>
      }
      stR(i)(j) = 1

      var ok = false

      // ε
      if (i == j) ok = true

      // a S b R
      if (!ok && i < j && input.charAt(i) == 'a') {
        var k = i + 1
        while (!ok && k <= j - 1) {
          if (input.charAt(k) == 'b') {
            if (canS(i + 1, k) && canR(k + 1, j)) ok = true
          }
          k += 1
        }
      }

      valR(i)(j) = ok
      stR(i)(j) = 2
      ok
    }

    private def matchLitAt(pos: Int, lit: String): Boolean = {
      val m = lit.length
      if (pos + m > n) return false
      var t = 0
      while (t < m) {
        if (input.charAt(pos + t) != lit.charAt(t)) return false
        t += 1
      }
      true
    }
  }

  // Random generation IN language
  final class InLangGenerator(maxLen: Int, rng: Random) {
    private val possibleR = Array.fill(maxLen + 1)(false)
    private val possibleS = Array.fill(maxLen + 1)(false)

    possibleR(0) = true
    private var len = 0
    while (len <= maxLen) {
      var okS = false
      if (possibleR(len)) okS = true
      if (!okS && len >= 3 && possibleR(len - 3)) okS = true
      if (!okS && len >= 6 && possibleR(len - 6)) okS = true
      possibleS(len) = okS

      if (len != 0) {
        var okR = false
        var s = 0
        while (!okR && s <= len - 2) {
          val r = len - 2 - s
          if (possibleS(s) && possibleR(r)) okR = true
          s += 1
        }
        possibleR(len) = okR
      }
      len += 1
    }

    def canGenerateS(len: Int): Boolean =
      len >= 0 && len <= maxLen && possibleS(len)

    def genSExact(len: Int): String = {
      require(canGenerateS(len), s"Length $len is not generatable by S within maxLen=$maxLen")

      val choicesB = mutable.ArrayBuffer.empty[Int]
      if (possibleR(len)) choicesB += 0
      if (len >= 3 && possibleR(len - 3)) choicesB += 3
      if (len >= 6 && possibleR(len - 6)) choicesB += 6

      val bLen = choicesB(rng.nextInt(choicesB.length))
      val sb = new StringBuilder(len)
      if (bLen == 3) sb.append("bab")
      else if (bLen == 6) sb.append("bbbbbb")
      sb.append(genRExact(len - bLen))
      sb.toString()
    }

    private def genRExact(len: Int): String = {
      if (len == 0) return ""

      val splits = mutable.ArrayBuffer.empty[(Int, Int)]
      splits.sizeHint(len)

      var sLen = 0
      while (sLen <= len - 2) {
        val rLen = len - 2 - sLen
        if (possibleS(sLen) && possibleR(rLen)) splits += ((sLen, rLen))
        sLen += 1
      }

      val (sChosen, rChosen) = splits(rng.nextInt(splits.length))
      "a" + genSExact(sChosen) + "b" + genRExact(rChosen)
    }
  }

  // Generate NOT-in-language
  private def flipChar(c: Char): Char = if (c == 'a') 'b' else 'a'

  private def randomWord(rng: Random, len: Int): String = {
    val sb = new StringBuilder(len)
    var i = 0
    while (i < len) {
      sb.append(if (rng.nextBoolean()) 'a' else 'b')
      i += 1
    }
    sb.toString()
  }

  private def genNotInLangSimple(
                                  len: Int,
                                  rng: Random,
                                  randomTries: Int = 2000,
                                  flipTriesPerHit: Int = 200
                                ): String = {

    require(len > 0, "len must be > 0")

    var tries = 0
    while (tries < randomTries) {
      val w0 = randomWord(rng, len)

      if (!NaiveRecursiveDescent.accepts(w0)) return w0

      val arr = w0.toCharArray
      var f = 0
      while (f < flipTriesPerHit) {
        val pos = rng.nextInt(len)
        arr(pos) = flipChar(arr(pos))
        val w1 = new String(arr)
        if (!NaiveRecursiveDescent.accepts(w1)) return w1
        f += 1
      }

      tries += 1
    }

    // Final fallback: guaranteed to fail the suffix-balance (end with 'a'), preserving length.
    "b" * (len - 1) + "a"
  }

  private def nowNs(): Long = System.nanoTime()
  private def nsToUs(ns: Double): Double = ns / 1e3

  private def benchmark(
                         seed: Long,
                         lengths: Seq[Int],
                         samplesPerLen: Int,
                         warmupPerLen: Int
                       ): Unit = {

    val rng = new Random(seed)
    val optimized = new OptimizedParser
    val gen = new InLangGenerator(maxLen = lengths.max, rng = rng)

    // Generate IN samples
    val inSamplesByLen: Map[Int, Vector[String]] =
      lengths.map { L =>
        require(gen.canGenerateS(L), s"Length $L is not generatable; pick another length.")
        val buf = Vector.newBuilder[String]
        buf.sizeHint(samplesPerLen)
        var cnt = 0
        while (cnt < samplesPerLen) {
          val w = gen.genSExact(L)
          if (NaiveRecursiveDescent.accepts(w)) { buf += w; cnt += 1 }
        }
        L -> buf.result()
      }.toMap

    // Generate NOT-IN samples
    val notSamplesByLen: Map[Int, Vector[String]] =
      lengths.map { L =>
        val buf = Vector.newBuilder[String]
        buf.sizeHint(samplesPerLen)
        var cnt = 0
        while (cnt < samplesPerLen) {
          val w = genNotInLangSimple(L, rng)
          if (!NaiveRecursiveDescent.accepts(w)) { buf += w; cnt += 1 }
        }
        L -> buf.result()
      }.toMap

    trait Parser { def name: String; def accepts(s: String): Boolean }
    val parsers: Vector[Parser] = Vector(
      new Parser { val name = "naive";     def accepts(s: String) = NaiveRecursiveDescent.accepts(s) },
      new Parser { val name = "optimized"; def accepts(s: String) = optimized.accepts(s) }
    )

    def verifyAll(title: String, samplesByLen: Map[Int, Vector[String]], expected: Boolean): Unit = {
      lengths.foreach { L =>
        val ws = samplesByLen(L)
        parsers.foreach { p =>
          var idx = 0
          while (idx < ws.length) {
            val w = ws(idx)
            val got = p.accepts(w)
            if (got != expected) {
              val expStr = if (expected) "ACCEPT" else "REJECT"
              val gotStr = if (got) "ACCEPT" else "REJECT"
              sys.error(
                s"Correctness check failed on $title: parser=${p.name}, expected=$expStr, got=$gotStr, len=$L, word=$w"
              )
            }
            idx += 1
          }
        }
      }
    }

    verifyAll("IN grammar", inSamplesByLen, expected = true)
    verifyAll("NOT IN grammar", notSamplesByLen, expected = false)

    def benchOneSet(title: String, samplesByLen: Map[Int, Vector[String]]): Unit = {
      lengths.foreach { L =>
        val ws = samplesByLen(L)
        var i = 0
        while (i < warmupPerLen) {
          val w = ws(i % ws.length)
          parsers.foreach(_.accepts(w))
          i += 1
        }
      }

      println(title)
      println(f"${"len"}%5s  ${"naive"}%12s  ${"optimized"}%12s  ${"naive/opt"}%12s")
      println("-" * 49)

      lengths.sorted.foreach { L =>
        val ws = samplesByLen(L)

        def avgNs(p: Parser): Double = {
          var idx = 0
          var acc = 0.0
          while (idx < ws.length) {
            val w = ws(idx)
            val t0 = nowNs()
            p.accepts(w)
            val t1 = nowNs()
            acc += (t1 - t0).toDouble
            idx += 1
          }
          acc / ws.length.toDouble
        }

        val tNaive = avgNs(parsers(0))
        val tOpt   = avgNs(parsers(1))

        println(
          f"$L%5d  ${nsToUs(tNaive)}%12.2f us  ${nsToUs(tOpt)}%12.2f us  ${tNaive / tOpt}%12.2f x"
        )
      }
      println()
    }

    benchOneSet("=== IN grammar ===", inSamplesByLen)
    benchOneSet("=== NOT IN grammar ===", notSamplesByLen)
  }

  def main(args: Array[String]): Unit = {
    val seed = 123456789L
    val lengths = Seq(60, 65, 70, 75, 80)
    val samplesPerLen = 2000
    val warmupPerLen = 300

    benchmark(seed, lengths, samplesPerLen, warmupPerLen)
  }
}

import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import scala.util.Random
import scala.util.matching.Regex

case class Ciphertext(c1: BigInt, c2: BigInt)

object CocksCipher {
  def generatePrime(bitLength: Int): BigInt = {
    val rnd = new Random()
    var prime = BigInt.probablePrime(bitLength, rnd)
    while (prime % 4 != 3) {
      prime = BigInt.probablePrime(bitLength, rnd)
    }
    prime
  }


  def jacobiSymbol(a: BigInt, n: BigInt): Int = {
    if (n <= 0 || n % 2 == 0) {
      throw new IllegalArgumentException("n must be odd and positive in jacobiSymbol.")
    }
    if (a % n == 0) return 0

    var aMod = a % n
    var nVar = n
    var result = 1

    while (aMod != 0) {
      while (aMod % 2 == 0) {
        aMod = aMod / 2
        val nRem8 = nVar % 8
        if (nRem8 == 3 || nRem8 == 5) result = -result
      }
      val tmp = aMod
      aMod = nVar
      nVar = tmp
      if (aMod % 4 == 3 && nVar % 4 == 3) result = -result
      aMod = aMod % nVar
    }

    if (nVar == 1) result else 0
  }


  def modInverse(a: BigInt, n: BigInt): BigInt = {
    a.modInverse(n)
  }


  def modPow(a: BigInt, e: BigInt, n: BigInt): BigInt = {
    a.modPow(e, n)
  }

  def makeHash(id: String, n: BigInt, maxIter: Int = 1000): BigInt = {
    var attempt: Option[BigInt] = None
    var i = 0
    while (i < maxIter && attempt.isEmpty) {
      val input = s"$id-$i".getBytes("UTF-8")
      val hashed = MessageDigest.getInstance("SHA-256").digest(input)
      val cand = BigInt(1, hashed) mod n

      if (jacobiSymbol(cand, n) == 1) {
        attempt = Some(cand)
      }
      i += 1
    }

    attempt.getOrElse {
      throw new RuntimeException(s"Cannot find a with (a/n)=1 in $maxIter iterations for ID=$id.")
    }
  }

  def parseCiphertexts(input: String): Seq[Ciphertext] = {
    val pattern: Regex = """Ciphertext\((\d+),(\d+)\)""".r

    val matches = pattern.findAllMatchIn(input)

    matches.map { m =>
      val c1 = BigInt(m.group(1))
      val c2 = BigInt(m.group(2))
      Ciphertext(c1, c2)
    }.toSeq
  }


  // Setup

  def setup(bitLength: Int): (BigInt, BigInt, BigInt) = {
    val p = generatePrime(bitLength)
    val q = generatePrime(bitLength)

    val n = p * q
    (n, p, q)
  }

  // Extract

  def extractR(id: String, n: BigInt, p: BigInt, q: BigInt): BigInt = {
    val a = makeHash(id, n)
    val exponent = (n + 5 - p - q) / 8
    val r = modPow(a, exponent, n)
    r
  }

  // Encrypt

  def encryptBit(m: Int, id: String, n: BigInt): Ciphertext = {
    require(m == 1 || m == -1, "m must be either 1 or -1")

    val a = makeHash(id, n)


    def randomT(m: Int): BigInt = {
      var t: BigInt = 0
      do {
        t = BigInt(n.bitLength, Random) % n
      } while (t <= 1 || jacobiSymbol(t, n) != m)
      t
    }

    // t1
    val t1 = randomT(m)
    val t1Inv = modInverse(t1, n)
    val c1 = (t1 + (a * t1Inv mod n)) mod n

    // t2 (отличный от t1)
    var t2: BigInt = 0
    do {
      t2 = randomT(m)
    } while (t2 == t1)

    val t2Inv = modInverse(t2, n)
    val c2 = (t2 - (a * t2Inv mod n)) mod n

    Ciphertext(c1, c2)
  }


  def encryptString(message: String, id: String, n: BigInt): Seq[Ciphertext] = {
    val bytes = message.getBytes("UTF-8")

    val bits: Seq[Int] = bytes.flatMap { b =>
      (0 until 8).reverse.map { i =>
        val bit = (b >> i) & 1
        if (bit == 1) 1 else -1
      }
    }.toSeq

    bits.map(bit => encryptBit(bit, id, n))
  }

  // Decrypt

  def decryptBit(s: Ciphertext, r: BigInt, id: String, n: BigInt): Int = {
    val a = makeHash(id, n)

    val r2 = (r * r) mod n
    val diff1 = (r2 - a) mod n
    val diff2 = (r2 + a) mod n


    val alpha =
      if (diff1 == 0) {
        // r^2 ≡ a (mod n)
        (s.c1 + 2*r) mod n
      } else if (diff2 == 0) {
        // r^2 ≡ -a (mod n)
        (s.c2 + 2*r) mod n
      } else {
        throw new RuntimeException("Invalid r: r^2 not equal to ±a mod n.")
      }

    jacobiSymbol(alpha, n)
  }

  def decryptString(ciphertexts: Seq[Ciphertext], r: BigInt, id: String, n: BigInt): String = {
    val bitValues: Seq[Int] = ciphertexts.map(ct => decryptBit(ct, r, id, n))
    val bytes: Array[Byte] =
      bitValues.grouped(8).map { groupOf8 =>
        var b: Int = 0
        for (i <- groupOf8.indices) {
          val bit = if (groupOf8(i) == 1) 1 else 0
          b = (b << 1) | bit
        }
        b.toByte
      }.toArray

    new String(bytes, "UTF-8")
  }
}


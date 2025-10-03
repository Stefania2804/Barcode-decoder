import Types.{Bit, Digit, Even, NoParity, Odd, One, Parity, Pixel, Str, Zero}

import scala.collection.immutable

object Decoder {
  // TODO 1.1
  def toBit(s: Char): Bit = {
    s match {
      case '0' => Zero
      case '1' => One
    }
  }
  def toBit(s: Int): Bit = {
    s match {
      case 0 => Zero
      case 1 => One
    }
  }

  // TODO 1.2
  def complement(c: Bit): Bit = {
    c match {
      case Zero => One
      case One => Zero
    }
  }

  // TODO 1.3
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
  val leftOddList: List[List[Bit]] = LStrings.map(_.toList.map(toBit)) // codificări L
  val rightList: List[List[Bit]] = LStrings.map(_.toList.map(char => complement(toBit(char))))// codificări R
  val leftEvenList: List[List[Bit]] = LStrings.map(_.toList.map(char => complement((toBit(char)))).reverse)// codificări  G
  
  // TODO 1.4
  def group[A](l: List[A]): List[List[A]] = {
    l.foldRight(List[List[A]]()) { (elem, acc) =>
      acc match {
        case Nil => List(List(elem))
        case x :: xs if x.head == elem => (elem :: x) :: xs
        case _ => List(elem) :: acc
      }
    }
  }

  // TODO 1.5
  def runLength[A](l: List[A]): List[(Int, A)] = {
    group(l).map(list => (list.length, list.head))
  }
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 2.1
    def -(other: RatioInt): RatioInt = {
      if (this.b == other.b) {
        RatioInt(this.a - other.a, this.b)
      } else {
        RatioInt(this.a * other.b - other.a * this.b, this.b * other.b)
      }
    }
    def +(other: RatioInt): RatioInt = {
      if (this.b == other.b) {
        RatioInt(this.a + other.a, this.b)
      } else {
        RatioInt(this.a * other.b + other.a * this.b, this.b * other.b)
      }
    }
    def *(other: RatioInt): RatioInt = {
      RatioInt(this.a * other.a, this.b * other.b)
    }
    def /(other: RatioInt): RatioInt = {
      RatioInt(this.a * other.b, this.b * other.a)
    }

    // TODO 2.2
    def compare(other: RatioInt): Int = {
      if (this.b == other.b) {
        if (this.a > other.a) 1
        else if (this.a < other.a) -1
        else 0
      } else {
        if (this.a * other.b > other.a * this.b) 1
        else if (this.a * other.b < other.a * this.b) -1
        else 0
      }
    }
  }
  
  // TODO 3.1
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
    def total(acc: Int, l: List[(Int, A)]): Int = {
      l match {
        case Nil => acc
        case (x, y) :: xs => total(acc + x, xs)
      }
    }
    l.map((ap, bit) => (RatioInt(ap, total(0, l)), bit))
  }

  // TODO 3.2
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
    val list = scaleToOne(l)
    (list.head._2, list.map ((ap_rel, _) => ap_rel))
  }
  
  // TODO 3.3
  def toParities(s: Str): List[Parity] = {
    s.map {
      case 'L' => Odd
      case 'G' => Even
      case _ => NoParity
    }
  }
  
  // TODO 3.4
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = PStrings.map(str => toParities(str.toList))

  // TODO 3.5
  type SRL = (Bit, List[RatioInt])
  val leftOddSRL:  List[SRL] = leftOddList.map(l => scaledRunLength(runLength(l)))
  val leftEvenSRL:  List[SRL] = leftEvenList.map(l => scaledRunLength(runLength(l)))
  val rightSRL:  List[SRL] = rightList.map(l => scaledRunLength(runLength(l)))

  // TODO 4.1
  def distance(l1: SRL, l2: SRL): RatioInt = {
    if (l1._1 != l2._1) RatioInt(1000, 1)
    else {
      l1._2.zip(l2._2).map {
        case (a, b) =>
          val result = a.compare(b)
          result match {
            case -1 => b - a
            case _ => a - b
          }
      }.foldLeft(RatioInt(0, 1))(_ + _)
    }
  }
  
  // TODO 4.2
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = {
    val RatioIntList: List[RatioInt] = SRL_Codes.map(pair => distance(pair, digitCode))
    (RatioIntList.min, RatioIntList.indexOf(RatioIntList.min))
  }
  
  // TODO 4.3
  def bestLeft(digitCode: SRL): (Parity, Digit) = {
    val bestL: (RatioInt, Digit) = bestMatch(leftOddSRL, digitCode)
    val bestG: (RatioInt, Digit) = bestMatch(leftEvenSRL, digitCode)
    bestL._1.compare(bestG._1) match {
      case 1 => (Even, bestG._2)
      case 0 => (Even, bestG._2)
      case -1 => (Odd, bestL._2)
    }
  }
  
  // TODO 4.4
  def bestRight(digitCode: SRL): (Parity, Digit) = {
    val bestR: (RatioInt, Digit) = bestMatch(rightSRL, digitCode)
    (NoParity, bestR._2)
  }

  def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
    l match {
      case Nil => Nil
      case _ =>
        val (h, t) = f(l)
        h :: chunkWith(f)(t)
    }
  }
  
  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] =
    chunkWith((l: List[A]) => l.splitAt(n))(l)

  // TODO 4.5
  def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = {
    def total(cnt: Int, list: List[(Int, Bit)]): Int = {
      list match {
        case Nil => cnt
        case (x, y) :: xs => total(cnt + 1, xs)
      }
    }
    if (total(0, rle) == 59) {
      val dropFirst: List[(Int, Bit)] = rle.drop(3)
      val dropLastAndFirst: List[(Int, Bit)] = dropFirst.dropRight(3)
      val (leftPart, rightPartWithMiddle) = dropLastAndFirst.splitAt(24)
      val rightPart: List[(Int, Bit)] = rightPartWithMiddle.drop(5)

      val leftGroups = chunksOf(4)(leftPart)
      val rightGroups = chunksOf(4)(rightPart)
      leftGroups.map(list => bestLeft(scaledRunLength(list))) ++ rightGroups.map(list => bestRight(scaledRunLength(list)))
    } else {
      List()
    }
  }

  // TODO 4.6
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = {
    val parityList: List[Parity] = l.map(_._1).dropRight(6)
    val list: List[(Boolean, Digit)] = leftParityList.map(listAux => listAux == parityList).zipWithIndex
    def loop(list2: List[(Boolean, Digit)]): Option[Digit] = {
      list2 match {
        case (bool, digit) :: xs if (bool == true) => Some(digit)
        case (bool, digit) :: xs if (bool == false) => loop(xs)
        case _ => None
      }
    }
    loop(list)
  }

  // TODO 4.7
  def checkDigit(l: List[Digit]): Digit = {
    val listWithIndex: List[(Digit, Int)] = l.zipWithIndex
    def sumControl(sum: Int, list: List[(Digit, Int)]): Int = {
      list match {
        case Nil => sum
        case x :: xs if ((x._2 + 1) % 2 == 0) => sumControl(sum + 3 * x._1, xs)
        case x :: xs if ((x._2 + 1) % 2 == 1) => sumControl(sum + x._1, xs)
        case _ => sum
      }
    }
    (10 - sumControl(0, listWithIndex) % 10) % 10
  }
  
  // TODO 4.8
  def verifyCode(code: List[(Parity, Digit)]): Option[String] = {
    if(code.length == 13) {
      if (Some(code.head._2) == firstDigit(code.drop(1)) && checkDigit(code.dropRight(1).map(_._2)) == code(code.length - 1)._2)
        println(Some(code.map(_._2.toString).mkString))
        Some(code.map(_._2.toString).mkString)
      else
        None
    } else {
      None
    }
  }
  
  // TODO 4.9
  def solve(rle:  List[(Int, Bit)]): Option[String] = {
    val list: List[(Parity, Digit)] = findLast12Digits(rle)
    val firstDig: Option[Digit] = firstDigit(list)
    firstDig match {
      case Some(digit) => Some(((NoParity, digit) :: list).map(_._2.toString).mkString)
      case None => None
    }
  }
  
  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, toBit(pair._2))))
  }
}



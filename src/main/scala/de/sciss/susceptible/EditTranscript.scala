/*
 *  EditTranscript.scala
 *  (Susceptible)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.susceptible

import de.sciss.kollflitz
import de.sciss.kollflitz.Vec

import scala.annotation.tailrec

object EditTranscript {
  final val Copy        = '-'
  final val Substitute  = 'S'
  final val Insert      = 'I'
  final val Delete      = 'D'

  implicit private final class VecOps[A](private val in: Vec[A]) extends AnyVal {
    def removeAt(index: Int         ): Vec[A] = in.patch(index, Nil        , 1)
    def insert  (index: Int, elem: A): Vec[A] = in.patch(index, elem :: Nil, 0)
  }

  def align(xs: Vec[String], fill: Char = '*'): Vec[String] =
    alignWith(xs.map(_.map(ch => (ch, ch))), fill = fill).map(_.mkString)

  def alignWith[A](xs: Vec[Vec[(Char, A)]], fill: A): Vec[Vec[A]] = {
    /*
        say the input is:

        "from"
        "chrome"
        "case"
        "place"
        "ice"
        "inner"

        then the pairwise transcripts are:

        "SI---I"
        "-SSDD-"
        "SI-S-"
        "SDD--"
        "-SI-I"

        the output would be:

                'S' 'I'    '---' 'I'     '-SS'  'DD'     'S'   'I'   '-S-'          'S'   'DD'         '-S'          'I-I'
        "from"      "f*rom"     "f*rom*" |    "f*rom*" |    "f**rom*"    "f**rom*" |    "f**rom*" |    "f**rom*"     "f**r*o*m*"
        "chrome" -> "chrome" -> "chrome" | -> "chrome" | -> "c*hrome" -> "c*hrome" | -> "c*hrome" | -> "c*hrome"  -> "c*hr*o*me"
        "case"      "case"      "case"   |    "cas**e" |    "c*as**e"    "c*as**e" |    "c*as**e" |    "c*as**e"     "c*as***e*"
        "place"     "place"     "place"  |    "place"  |    "place"      "plac**e" |    "plac**e" |    "plac**e"     "plac***e*"
        "ice"       "ice"       "ice"    |    "ice"    |    "ice"        "ice"     |    "i**c**e" |    "i**c**e"     "i**c***e*"
        "inner"     "inner"     "inner"  |    "inner"  |    "inner"      "inner"   |    "inner"   |    "i**nner"     "i**nn**er"

        And the last action can be improved: an 'I' where a place holder '*' is found is "swalled". Thus:

        "f**rom**"
        "c*hrome*"
        "c*as**e*"
        "plac**e*"
        "i**c**e*"
        "i**nn*er"

        So the algorithm is as follows:

        - push head so seq A
        - pair tail with transcripts as B
        - foreach pair (x, t) in B, zip it with a = A.last, generate new row y
          - look at t
            - '-' or 'S': while a contains placeholder, insert placeholder in y, then skip in a / copy from x to y
            - 'I': if a contains placeholder, then skip in a / copy from x to y
                   if not, insert placeholder in a and all preceding vectors, copy from x to y
            - 'D': skip in a, insert placeholder in y
           - finally pad y to length of a
         - append y to A and repeat with previous tail
     */

    import kollflitz.Ops._
    val paired = xs.mapPairs { (pred, succ) =>
      val (predChar, _    ) = pred.unzip
      val (succChar, succA) = succ.unzip
      val trans = apply(predChar.mkString, succChar.mkString)
      (succA, trans)
    }

    // This is terrible to write with tail recursive functions. We resort to loops...
    var done = xs.take(1).map(_.map(_._2))
    paired.foreach { case (x, t) =>
      var a     = done.last
      var aIdx  = 0
      var xIdx  = 0
      var y     = Vector.empty[A]
      t.foreach {
        case Copy | Substitute =>
          while (a(aIdx) == fill) {
            aIdx += 1
            y   :+= fill
          }
          aIdx += 1
          y   :+= x(xIdx)
          xIdx += 1

        case Insert =>
          if (aIdx == a.length || a(aIdx) != fill) {
            done  = done.map(_.insert(aIdx, fill))  // insert placeholder
            a     = done.last
          }
          aIdx += 1
          y   :+= x(xIdx)
          xIdx += 1

        case Delete =>
          aIdx += 1
          y   :+= fill
      }

      done :+= y.padTo(a.length /* aIdx */, fill)
    }

    done
  }

  /** Calculates the edit transcript from one string `a` to another `b`,
    * trying to minimise the cost of operations.
    *
    * @return the transcript string where '-' is copy, 'D' is delete, 'I' is insert, 'S' is substitute
    */
  def apply(a: String, b: String): String = {
    val aLen  = a.length
    val bLen  = b.length
    val dist : Array[Array[Int ]] = Array.ofDim(aLen + 1, bLen + 1)
    val trans: Array[Array[Char]] = Array.ofDim(aLen + 1, bLen + 1)

    def min(a: Int, b: Int, c: Int): Int = math.min(a, math.min(b, c))

    // initialise matrices
    for (i <- 0 to a.length) {
      dist (i)(0) = i
      trans(i)(0) = 'D'
    }
    for (i <- 0 to bLen) {
      dist (0)(i) = i
      trans(0)(i) = 'I'
    }

    for (i <- 1 to aLen) {
      for (j <- 1 to bLen) {
        val same  = a.charAt(i - 1) == b.charAt(j - 1)
        val cIns  = dist(i    )(j - 1) + 1
        val cDel  = dist(i - 1)(j    ) + 1
        val cAlt  = dist(i - 1)(j - 1) + (if (same) 0 else 1)

        val cBest = min(cIns, cDel, cAlt)
        dist(i)(j) = cBest
        val edit =
          if      (cBest == cIns) 'I'
          else if (cBest == cDel) 'D'
          else if (same)          '-'
          else                    'S'

        trans(i)(j) = edit
      }
    }

    // go back in matrix to create transcript
    @tailrec
    def trace(i: Int, j: Int, res: List[Char]): List[Char] =
    if (i == 0 && j == 0) res else {
      val edit  = trans(i)(j)
      val res1  = edit :: res
      val isAlt = edit == 'S' || edit == '-'
      val i1    = if (i > 0 && (isAlt || edit == 'D')) i - 1 else i
      val j1    = if (j > 0 && (isAlt || edit == 'I')) j - 1 else j
      trace(i1, j1, res1)
    }

    val st = trace(aLen, bLen, Nil)
    st.mkString
  }
}
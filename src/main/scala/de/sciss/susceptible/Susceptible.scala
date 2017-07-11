/*
 *  Susceptible.scala
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

import java.awt.Font
import java.io.FileInputStream

import de.sciss.file._
import de.sciss.kollflitz.Vec
import org.pegdown.{PegDownProcessor, ast}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Susceptible {
  final case class Config(textFile: File, fontName: String = "DejaVu Sans Mono", fontSize: Int = 36)

  def main(args: Array[String]): Unit = {
    val c = Config(textFile = file("/data/projects/Transpositions/chapter/chapter.md"))
    run(c)
  }

  def readAllText(f: File): String = {
    val szL = f.length()
    require (szL <= 0x7FFFFFF)
    val sz  = szL.toInt
    val b   = new Array[Byte](sz)
    var off = 0
    val in  = new FileInputStream(f)
    try {
      while (in.available() > 0) {
        off += in.read(b, off, in.available())
      }
      new String(b, 0, off, "UTF-8")
    } finally {
      in.close()
    }
  }

  implicit class SuperNodeOps(private val n: ast.SuperNode) extends AnyVal {
    def children: Seq[ast.Node] = n.getChildren.asScala
  }

  def run(config: Config): Unit = {
    val text  = readAllText(config.textFile)
    val peg   = new PegDownProcessor(10000L)
    val root  = peg.parseMarkdown(text.toCharArray)

    /*
      at the root level, we find:

      - HeaderNode
      - ParaNode
      - BlockQuoteNode
      - SimpleNode (type = HRule)

      in ParaNode we find

      - SuperNode. Inside this we find

        - TextNode
        - StrongEmphSuperNode, containing a TextNode

      in BlockQuoteNode we find

      - ParaNode

      in HeaderNode we find

      - TextNode

     */

    import de.sciss.kollflitz.Ops._

    // a section begins with a header and
    // proceeds until another header is found
    val sections = root.children.groupWith {
      case (_, _: ast.HeaderNode) => false
      case _ => true
    } .toList

    def mkWords(text: String): Seq[String] = {
      val t1 = text.replaceAll("""\(.*?\) ?""", "") // remove stuff in parentheses
      val t2 = t1  .replaceAll("""—""", """ — """)  // make sure we split around m-dashes
      val t3 = t2.split("""\s""").toIndexedSeq      // split around white space

      @tailrec
      def glueOrphans(in: Vec[String], orphan: String): Vec[String] = {
        val i = in.indexOf(orphan) - 1
        if (i < 0) in else {
          val pre   = in(i)
          val newIn = in.patch(i, s"$pre$orphan" :: Nil, 2)
          glueOrphans(newIn, orphan)
        }
      }

      val t4 = t3.filterNot(_.isEmpty)
      val t5 = glueOrphans(t4, ".")
      val t6 = glueOrphans(t5, ",")
      t6
    }

    def mkParagraphs(in: Seq[ast.Node]): Seq[Seq[String]] = in.flatMap {
      case n: ast.ParaNode =>
        n.children.flatMap {
          case nn: ast.SuperNode =>
            def inner(p: ast.Node): Seq[String] = p match {
              case pc: ast.TextNode             => pc.getText :: Nil
              case pc: ast.StrongEmphSuperNode  => pc.children.flatMap(inner)
            }

            val paraText = nn.children.flatMap(inner).mkString
            mkWords(paraText)

          case _ => Nil
        } :: Nil

      case n: ast.BlockQuoteNode => mkParagraphs(n.children)
      case _ => Nil
    }

    // sections -> paragraphs -> words
    val sectionsP: Seq[Seq[Seq[String]]] = sections.map(mkParagraphs)

    val section1 = sectionsP.head

    val par1 = section1.head

    val fnt = new Font(config.fontName, Font.PLAIN, config.fontSize)
    val sim = new GlyphSimilarity(fnt)

    val cmp: Iterator[(Double, String, String)] =
      par1.combinations(2).map { case Seq(word1, word2) =>
        val Seq(word1A, word2A) = EditTranscript.align(Vec(word1, word2), fill = ' ')
        val res = sim.compare(word1A, word2A)
        (res, word1, word2)
      }

    cmp.toSeq.sortBy(_._1).foreach { case (res, word1, word2) =>
      println(f"$res%g: $word1, $word2")
    }
  }
}
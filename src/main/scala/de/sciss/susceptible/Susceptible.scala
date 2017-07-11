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
import javax.swing.{KeyStroke, SpinnerNumberModel}

import de.sciss.desktop.{FileDialog, OptionPane}
import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.susceptible.force.Visual
import de.sciss.swingplus.Spinner
import org.pegdown.{PegDownProcessor, ast}
import prefuse.util.ui.JForcePanel
import scopt.OptionParser

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.{breakOut, mutable}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Component, FlowPanel, Frame, Label, Menu, MenuBar, MenuItem, Orientation, ProgressBar, Swing, ToggleButton}
import scala.util.Failure

object Susceptible {
  /**
    * @param textFile   a markdown file containing the text to render
    */
  final case class Config(textFile    : File    = file("chapter.md"),
                          fontName    : String  = "DejaVu Sans Mono",
                          fontSize    : Int     = 36,
                          imageWidth  : Int     = 4096,
                          imageHeight : Int     = 4096,
                          section     : Int     = 0
                         )

  def main(args: Array[String]): Unit = {
    val p = new OptionParser[Config]("Susceptible") {
      opt[File]('i', "text-input") required() text "input text file (markdown)" action { case (v, c) => c.copy(textFile = v) }
      opt[String]("font-name")      action { case (v, c) => c.copy(fontName     = v) }
      opt[Int]("font-size" )        action { case (v, c) => c.copy(fontSize     = v) }
      opt[Int]('w', "image-width")  action { case (v, c) => c.copy(imageWidth   = v) }
      opt[Int]('h', "image-height") action { case (v, c) => c.copy(imageHeight  = v) }
      opt[Int]('s', "section") text "section within the markdown file (counting from zero)" action { case (v, c) => c.copy(section = v) }
    }
    p.parse(args, Config()).fold(sys.exit(1)) { config =>
      run(config)
    }
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

    val section = sectionsP(config.section)

    val fnt = new Font(config.fontName, Font.PLAIN, config.fontSize)
    val gs  = new GlyphSimilarity(fnt)

    val edges: Visual.WordEdges = section.flatMap { par =>
      val cmpIt: Iterator[(Double, String, String)] =
        par.combinations(2).map { case Seq(word1, word2) =>
          val Seq(word1A, word2A) = EditTranscript.align(Vec(word1, word2), fill = ' ')
          val sim = gs.compare(word1A, word2A)
          (sim, word1, word2)
        }

      val cmp = cmpIt.toList
      val edges = cmp.map { case (sim, word1, word2) =>
        Edge(start = word1, end = word2, weight = 1.0 - sim)
      }

      val mst: List[Edge[String]] = MSTKruskal[String, Edge[String]](edges)
      fixWords(mst)
    } (breakOut)

    gs.dispose()

    Swing.onEDT {
      mkFrame(config, edges)
    }
  }

  def fixWords(edges: List[Edge[String]]): Visual.WordEdges = {
    val wordMap = mutable.Map.empty[String, Visual.Word]
    val edgesW = edges.map { edge =>
      val w1 = wordMap.getOrElseUpdate(edge.start, Visual.Word(edge.start))
      val w2 = wordMap.getOrElseUpdate(edge.end  , Visual.Word(edge.end  ))
      Edge(start = w1, end = w2, weight = edge.weight)
    }
    edgesW
  }

  def mkFrame(config: Config, edges: Visual.WordEdges): Unit = {
    val v = Visual()
    // v.display.setDoubleBuffered(true)
    v.displaySize = (800, 800)

    v.text = edges

    lazy val ggAutoZoom: ToggleButton = new ToggleButton("Zoom") {
      selected = true
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          v.autoZoom = selected
      }
    }

    lazy val ggRunAnim: ToggleButton = new ToggleButton("Anim") {
      selected = true
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          v.runAnimation = selected
      }
    }

    val mOutline = new SpinnerNumberModel(0, 0, 10, 1)
    lazy val ggOutline: Spinner = new Spinner(mOutline) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          v.textOutline = mOutline.getNumber.intValue()
      }
    }

    val mWidth = new SpinnerNumberModel(config.imageWidth, 16, 16384, 1)
    val ggWidth: Spinner = new Spinner(mWidth)

    val mHeight = new SpinnerNumberModel(config.imageHeight, 16, 16384, 1)
    val ggHeight: Spinner = new Spinner(mHeight)

    lazy val pBottom: Component = new BoxPanel(Orientation.Vertical) {
      contents += new FlowPanel(ggAutoZoom, ggRunAnim, new Label("Outline:"), ggOutline,
        new Label("Width:"), ggWidth, new Label("Height:"), ggHeight)
    }
    lazy val pRight: BoxPanel = new BoxPanel(Orientation.Vertical) {
      contents += VStrut(16)  // will be replaced
//      contents += cfgView.component
//      contents += ggText
    }

    // stupidly, it doesn't listen for model changes
    def mkForcePanel(): Unit = {
      val fSim    = v.forceSimulator
      val fPanel  = new JForcePanel(fSim)
      fPanel.setBackground(null)
      pRight.contents.update(0, Component.wrap(fPanel))
    }

    mkForcePanel()

    val split = new BorderPanel {
      add(v.component, BorderPanel.Position.Center )
      add(pBottom    , BorderPanel.Position.South )
    }

    //    split.oneTouchExpandable  = true
    //    split.continuousLayout    = false
    //    split.dividerLocation     = 800
    //    split.resizeWeight        = 1.0

    val mb = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Export Image...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl S"))
          def apply(): Unit = {
            FileDialog.save(init = Some(config.textFile.replaceExt("png"))).show(None).foreach { f =>
              val pFull = new RenderImage(v,
                width   = mWidth .getNumber.intValue,
                height  = mHeight.getNumber.intValue,
                fOut = f.replaceExt("png"))
              pFull.start()
              val futTail = pFull
//              val futTail = pFull.map { _ =>
//                val json = Situation.format.writes(sit).toString()
//                blocking {
//                  val jsonOut = new FileOutputStream(f.replaceExt("json"))
//                  jsonOut.write(json.getBytes("UTF-8"))
//                  jsonOut.close()
//                }
//              }
              mkProgressDialog("Exporting...", pFull, futTail)
            }
          }
        })
      }
    }

    new Frame {
      title     = "Text"
      contents  = new BorderPanel {
        add(split   , BorderPanel.Position.Center)
        // add(pBottom , BorderPanel.Position.South)
        add(pRight  , BorderPanel.Position.East)
      }
      menuBar = mb
//      resizable = false
      pack().centerOnScreen()
      // size      = (640, 480)

      // v.display.panTo((-136 + 20, -470 + 20))   // XXX WAT -- where the heck do these values come from?
      //      v.display.panTo((-100, 100))
      //      v.display.zoomAbs((0, 0), 1.3333)

      open()

      override def closeOperation(): Unit = {
        try {
          // v.algorithm.system.close()
        } finally {
          sys.exit(0)
        }
      }
    }

    v.display.panAbs(400, 400)
    v.runAnimation = true
  }

  private final class RenderImage(v: Visual, width: Int, height: Int, fOut: File)
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = blocking {
//      if (!fOut.exists()) {
        v.saveFrameAsPNG(fOut, width = width, height = height)
//      }
      progress = 1.0
    }
  }

  def mkProgressDialog(title: String, p: Processor[Any], tail: Future[Any]): Unit = {
    val ggProg  = new ProgressBar
    val ggAbort = new Button("Abort")
    val opt     = OptionPane(message = ggProg, messageType = OptionPane.Message.Plain, entries = Seq(ggAbort))

    val optPeer = opt.peer
    val dlg = optPeer.createDialog(title)
    ggAbort.listenTo(ggAbort)
    ggAbort.reactions += {
      case ButtonClicked(_) =>
        p.abort()
    }
    tail.onComplete(_ => onEDT(dlg.dispose()))
    tail.onComplete {
      case Failure(Processor.Aborted()) =>
      case Failure(ex) => ex.printStackTrace()
      case _ =>
    }
    p.addListener {
      case prog @ Processor.Progress(_, _) => onEDT(ggProg.value = prog.toInt)
    }
    dlg.setVisible(true)
  }
}
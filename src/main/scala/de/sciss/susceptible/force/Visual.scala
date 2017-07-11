/*
 *  Visual.scala
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

package de.sciss.susceptible.force

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Graphics, LayoutManager, RenderingHints}
import javax.imageio.ImageIO
import javax.swing.JPanel

import de.sciss.file._
import de.sciss.kollflitz
import prefuse.action.assignment.ColorAction
import prefuse.action.{ActionList, RepaintAction}
import prefuse.activity.Activity
import prefuse.controls.{DragControl, PanControl, WheelZoomControl, ZoomControl}
import prefuse.data.{Graph => PGraph}
import prefuse.render.DefaultRendererFactory
import prefuse.util.ColorLib
import prefuse.util.force.{DragForce, Force, ForceSimulator}
import prefuse.visual.expression.InGroupPredicate
import prefuse.visual.{VisualGraph, VisualItem}
import prefuse.{Constants, Display, Visualization}

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.swing.{Component, Dimension, Graphics2D, Rectangle}
import scala.util.Random

object Visual {
  val DEBUG = false

  private lazy val _initFont: Font = {
    // val is  = new FileInputStream("dosis/Dosis-Medium.ttf")
    // val name  = "vesper_libre/VesperLibre-Regular.ttf"
    val name  = "roboto/Roboto-Medium.ttf"
    val is    = getClass.getClassLoader.getResourceAsStream(name)
    val res   = Font.createFont(Font.TRUETYPE_FONT, is)
    is.close()
    res
  }

  private var _condensedFont: Font = _

  /** A condensed font for GUI usage. This is in 12 pt size,
    * so consumers must rescale.
    */
  def condensedFont: Font = {
    if (_condensedFont == null) _condensedFont = _initFont
    _condensedFont
  }
  def condensedFont_=(value: Font): Unit =
    _condensedFont = value

  final         val GROUP_GRAPH   = "graph"
  final         val COL_MUTA      = "muta"

  private final val GROUP_NODES   = "graph.nodes"
  private final val GROUP_EDGES   = "graph.edges"
  private final val ACTION_LAYOUT = "layout"
  private final val ACTION_COLOR  = "color"
  private final val LAYOUT_TIME   = 50

  def apply(): Visual = {
    val res = new Impl
    res.guiInit()
    res
  }

  class Word(val letters: Vec[VisualVertex], val word: String) {
    def dispose(): Unit = letters.foreach(_.dispose())
    lazy val width: Int = letters.map(_.advance).sum   // yes I know this is not precise
  }

  class Line(val words: Vec[Word])

  private final class Impl // (map: TMap[S#ID, VisualVertex], val algorithm: Algorithm, cursorPos0: S#Acc)
    extends Visual /* with ComponentHolder[Component] */ {

    private[this] var _vis: Visualization       = _
    private[this] var _dsp: MyDisplay           = _
    private[this] var _g  : PGraph              = _
    private[this] var _vg : VisualGraph         = _
    private[this] var _lay: MyForceDirectedLayout = _
    private[this] var actionColor: ActionList   = _
    private[this] var actionAutoZoom: AutoZoom  = _

    private[this] var _runAnim    = false

    private[this] var _text       = ""
    private[this] var wordMap     = Map.empty[String, List[Word]]
    private[this] var wordVec     = Vec.empty[Word]

    private[this] var forces: Map[String, Force] = _

    private[this] var _autoZoom   = true

    private[this] val rnd         = new Random()
    private[this] val _nBody      = new NBodyForce
    private[this] val _spring     = new MySpringForce

    private[this] var _lineWidth  = 320

    def autoZoom: Boolean = _autoZoom
    def autoZoom_=(value: Boolean): Unit = if (_autoZoom != value) {
      _autoZoom = value
      runAnimation = !runAnimation
      runAnimation = !runAnimation
    }

    def forceSimulator: ForceSimulator = _lay.getForceSimulator

    def display       : Display       = _dsp
    def visualization : Visualization = _vis
    def graph         : PGraph        = _g
    def visualGraph   : VisualGraph   = _vg

    private def visDo[A](body: => A): A = _vis.synchronized {
      // stopAnimation()
      val res = body
      // startAnimation()
      res
    }

    def text: String = _text
    def text_=(value: String): Unit = if (_text != value) setText1(value)

    def setSeed(n: Long): Unit = {
      rnd.setSeed(n)
      _nBody .setSeed(rnd.nextLong())
      _spring.setSeed(rnd.nextLong())
    }

    def lineWidth: Int = _lineWidth
    def lineWidth_=(value: Int): Unit = if (_lineWidth != value) {
      _lineWidth = value
      setText1(_text)
    }

    private def setText1(value: String) =
      visDo {
        stopAnimation()
        setText(value)
        startAnimation()
      }

    private def setText(value: String): Unit = {
      _text = value

      import kollflitz.Ops._
      val x0      = value.replace('\n', ' ').replace("  ", " ")
      val words   = x0.toVector.groupWith { (a, b) =>
        a.isLetterOrDigit && b.isLetterOrDigit
      } .map(_.mkString).toVector

      wordVec.foreach(_.dispose())
      wordMap = Map.empty
      wordVec = Vector.empty

      import kollflitz.Ops._

      val lineRef0 = new AnyRef

      val ws = words.map { word =>
        val wordRef = new AnyRef
        val vs      = word.map { c =>
          val vv = VisualVertex(this, lineRef = lineRef0, wordRef = wordRef, character = c)
          vv
        }

        vs.foreachPair { (pred, succ) =>
          graph.addEdge(pred.pNode, succ.pNode)
        }
        val w  = new Word(vs, word)
        wordMap += word -> (w :: wordMap.getOrElse(word, Nil))
        wordVec :+= w
        w
      }

      @tailrec def mkLines(words: Vec[Word], rem: Vec[Word], width: Int, res: Vec[Line]): Vec[Line] = {
        def flush(): Line = {
          words.foreachPair { (pred, succ) =>
            val n1 = pred.letters.last.pNode
            val n2 = succ.letters.head.pNode
            graph.addEdge(n1, n2)
          }
          val line = new Line(words)
          words.foreach(_.letters.foreach(_.lineRef = line))
          line
        }

        // note: we allow the last word to exceed the maximum width
        if (width > _lineWidth && rem.headOption.map(_.letters.size).getOrElse(0) > 1) {
          val line = flush()
          mkLines(Vector.empty, rem, 0, res :+ line)
        } else rem match {
          case head +: tail =>
            mkLines(words :+ head, tail, width + head.width, res)
          case _ =>
            val line = flush()
            res :+ line
        }
      }

      val lines = mkLines(Vector.empty, ws, 0, Vector.empty)

      lines.foreachPair { (pred, succ) =>
        val n1 = pred.words.head.letters.head.pNode
        val n2 = succ.words.head.letters.head.pNode
        graph.addEdge(n1, n2)
        //          val n3 = predL.letters.last.pNode
        //          val n4 = succL.letters.last.pNode
        //          graph.addEdge(n3, n4)
      }
    }

    @inline private def stopAnimation(): Unit = {
      _vis.cancel(ACTION_COLOR )
      _vis.cancel(ACTION_LAYOUT)
    }

    @inline private def startAnimation(): Unit =
      _vis.run(ACTION_COLOR)

    private def mkActionColor(): Unit = {
      // colors
      val actionNodeStrokeColor  = new ColorAction(GROUP_NODES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      actionNodeStrokeColor.add(VisualItem.HIGHLIGHT, ColorLib.rgb(255, 255, 0))
      // actionNodeStroke.add(VisualItem.HIGHLIGHT, ColorLib.rgb(220, 220, 0))
      val actionNodeFillColor    = new ColorAction(GROUP_NODES, VisualItem.FILLCOLOR  , ColorLib.rgb(0, 0, 0))
      actionNodeFillColor.add(VisualItem.HIGHLIGHT, ColorLib.rgb(63, 63, 0))
      val actionTextColor   = new ColorAction(GROUP_NODES, VisualItem.TEXTCOLOR  , ColorLib.rgb(255, 255, 255))

      val actionEdgeStrokeColor = new ColorAction(GROUP_EDGES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      val actionEdgeFillColor   = new ColorAction(GROUP_EDGES, VisualItem.FILLCOLOR  , ColorLib.rgb(255, 255, 255))
      //      val actionAggrFill    = new ColorAction(AGGR_PROC  , VisualItem.FILLCOLOR  , ColorLib.rgb(80, 80, 80))
      //      val actionAggrStroke  = new ColorAction(AGGR_PROC  , VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))

      actionColor = new ActionList(_vis)
      actionColor.add(actionTextColor)
      actionColor.add(actionNodeStrokeColor)
      actionColor.add(actionNodeFillColor)
      actionColor.add(actionEdgeStrokeColor)
      actionColor.add(actionEdgeFillColor  )
      //      actionColor.add(actionAggrFill)
      //      actionColor.add(actionAggrStroke)
      // actionColor.add(_lay)
      _vis.putAction(ACTION_COLOR, actionColor)
    }

    def guiInit(): Unit = {
      _vis = new Visualization
      _dsp = new MyDisplay(_vis) {
        override def setRenderingHints(g: Graphics2D): Unit = {
          super.setRenderingHints(g)
          g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
            if (m_highQuality) RenderingHints.VALUE_STROKE_PURE else RenderingHints.VALUE_STROKE_NORMALIZE)
        }

//        private var img2: BufferedImage = _
//        private var img3: BufferedImage = _

        override def paintBufferToScreen(g: Graphics): Unit = this.synchronized {
          val img = m_offscreen
          g.drawImage(img, 0, 0, getWidth, getHeight, null)
        }
      }

      _g     = new PGraph(true)
      _vg    = _vis.addGraph(GROUP_GRAPH, _g)
      _vg.addColumn(COL_MUTA, classOf[AnyRef])

      val procRenderer = new BoxRenderer(this) // new NuagesShapeRenderer(50)
      val edgeRenderer = new MyEdgeRenderer
      // edgeRenderer.setArrowHeadSize(8, 8)

      val rf = new DefaultRendererFactory(procRenderer)
      rf.add(new InGroupPredicate(GROUP_EDGES), edgeRenderer)
      // rf.add(new InGroupPredicate(AGGR_PROC  ), aggrRenderer)
      _vis.setRendererFactory(rf)

      _lay = new MyForceDirectedLayout(this)

      val sim = new ForceSimulator
      sim.addForce(_nBody)
      sim.addForce(_spring)
      sim.addForce(new DragForce)
      _lay.setForceSimulator(sim)
      forces = sim.getForces.map { f => (f.getClass.getSimpleName, f) } (breakOut)

      //      val forceMap = Map(
      //        ("NBodyForce" , "GravitationalConstant") -> -2.0f,
      //        ("DragForce"  , "DragCoefficient"      ) -> 0.002f,
      //        ("SpringForce", "SpringCoefficient"    ) -> 1.0e-5f,
      //        ("SpringForce", "DefaultSpringLength"  ) -> 200.0f
      //      )

      //      val forceMap = Map(
      //        ("NBodyForce" , "GravitationalConstant") -> -10.0f,
      //        ("NBodyForce" , "Distance"             ) -> -1.0f,
      //        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.57f,
      //        ("DragForce"  , "DragCoefficient"      ) -> 0.023f,
      //        ("SpringForce", "SpringCoefficient"    ) -> 1.0e-5f,
      //        ("SpringForce", "DefaultSpringLength"  ) -> 200.0f
      //      )

      //      val forceMap = Map(
      //        ("NBodyForce" , "GravitationalConstant") -> -2.0f,
      //        ("NBodyForce" , "Distance"             ) -> -1.0f,
      //        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.57f,
      //        ("DragForce"  , "DragCoefficient"      ) -> 0.01f,
      //        ("SpringForce", "SpringCoefficient"    ) -> 1.0e-5f,
      //        ("SpringForce", "DefaultSpringLength"  ) -> 10.0f
      //      )

      //      val forceMap = Map(
      //        ("NBodyForce" , "GravitationalConstant") -> -0.1f,
      //        ("NBodyForce" , "Distance"             ) -> -1.0f,
      //        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.4f,
      //        ("DragForce"  , "DragCoefficient"      ) -> 0.02f,
      //        ("MySpringForce", "SpringCoefficient"  ) -> 8.0e-5f,
      //        ("MySpringForce", "DefaultSpringLength") -> 0.1f, // 150.0f,
      //        ("MySpringForce", "Torque"             ) -> 2.0e-4f,
      //        ("MySpringForce", "Limit"              ) -> 300.0f
      //      )

      val forceMap = Map(
        ("NBodyForce" , "GravitationalConstant") -> 0f, // -0.01f,
        ("NBodyForce" , "Distance"             ) -> -1.0f,
        ("NBodyForce" , "BarnesHutTheta"       ) -> 0.4f,
        ("DragForce"  , "DragCoefficient"      ) -> 0.015f,
        ("MySpringForce", "SpringCoefficient"  ) -> 1.0e-4f,
        ("MySpringForce", "VSpringCoefficient" ) -> 1.0e-4f,
        // ("MySpringForce", "DefaultSpringLength") -> 0.1f, // 150.0f,
        ("MySpringForce", "HTorque"            ) -> 2.0e-4f,
        ("MySpringForce", "VTorque"            ) -> 2.0e-4f,
        ("MySpringForce", "Limit"              ) -> 300.0f
      )

      forceSimulator.getForces.foreach { force =>
        val fName = force.getClass.getSimpleName
        // println(s"----FORCE----$fName")
        for (i <- 0 until force.getParameterCount) {
          val pName = force.getParameterName(i)
          forceMap.get((fName, pName)).foreach { value =>
            force.setParameter(i, value)
          }
          // println(pName)

          // NBodyForce
          // - GravitationalConstant = -2.0
          // - Distance = -1.0
          // - BarnesHutTheta = 0.89
          // DragForce
          // - DragCoefficient = 0.002
          // SpringForce
          // - SpringCoefficient = 1.0e-5
          // - DefaultSpringLength = 200.0
        }
      }

      _lay.setMaxTimeStep(40L)
      _lay.setIterations(1)
      // forceSimulator.setSpeedLimit(0.025f)

      // ------------------------------------------------

      actionAutoZoom = new AutoZoom(this)

      // initialize the display
      _dsp.addControlListener(new ZoomControl     ())
      _dsp.addControlListener(new WheelZoomControl())
      _dsp.addControlListener(new PanControl        )
      _dsp.addControlListener(new DragControl       )
      _dsp.addControlListener(new ClickControl(this))
      _dsp.setHighQuality(true)

      // ------------------------------------------------

      edgeRenderer.setHorizontalAlignment1(Constants.CENTER) // LEFT )
      edgeRenderer.setHorizontalAlignment2(Constants.CENTER) // RIGHT)
      edgeRenderer.setVerticalAlignment1  (Constants.CENTER)
      edgeRenderer.setVerticalAlignment2  (Constants.CENTER)

      _dsp.setForeground(Color.WHITE)
      _dsp.setBackground(Color.BLACK)

      //      setLayout( new BorderLayout() )
      //      add( display, BorderLayout.CENTER )
      val p = new JPanel
      p.setLayout(new Layout(_dsp))
      p.add(_dsp)

      mkAnimation()
      // _vis.run(ACTION_COLOR)

      component = Component.wrap(p)
    }

    def displaySize: Dimension = _dsp.getSize

    def displaySize_=(value: Dimension): Unit = {
      _dsp.setSize(value)
      _dsp.setMinimumSize(value)
      _dsp.setMaximumSize(value)
    }

    def imageSize: Dimension = new Dimension(_dsp.bufWidth, _dsp.bufHeight)

    def imageSize_=(value: Dimension): Unit = {
      _dsp.bufWidth   = value.width
      _dsp.bufHeight  = value.height
    }

    var component: Component = _

    def forceParameters: Map[String, Map[String, Float]] = forces.map { case (name, force) =>
      val values: Map[String, Float] = (0 until force.getParameterCount).map { i =>
        (force.getParameterName(i), force.getParameter(i))
      } (breakOut)
      (name, values)
    }

    def forceParameters_=(params: Map[String, Map[String, Float]]): Unit =
      forceSimulator.getForces.foreach { force =>
        val fName = force.getClass.getSimpleName
        val map   = params.getOrElse(fName, Map.empty)
        for (i <- 0 until force.getParameterCount) {
          val pName = force.getParameterName(i)
          val valOpt: Option[Float] = map.get(pName)
          valOpt.foreach { value =>
            if (force.getParameter(i) != value) {
              if (DEBUG) println(s"$fName - $pName - $value")
              force.setParameter(i, value)
            }
          }
        }
      }

    def layoutCounter: Int = _lay.counter

    def runAnimation: Boolean = _runAnim
    def runAnimation_=(value: Boolean): Unit = if (_runAnim != value) {
      // requireEDT()
      _vis.synchronized {
        stopAnimation()
        _vis.removeAction(ACTION_COLOR)
        _vis.removeAction(ACTION_LAYOUT)
        _runAnim = value
        mkAnimation()
      }
    }

    private def mkAnimation(): Unit = {
      mkActionColor() // create really new instance, because `alwaysRunsAfter` installs listener on this
      val actionLayout = if (_runAnim) {
        new ActionList(Activity.INFINITY, LAYOUT_TIME)
      } else {
        new ActionList()
      }
      actionLayout.add(_lay)
      if (autoZoom) actionLayout.add(actionAutoZoom)
      // actionLayout.add(new PrefuseAggregateLayout(AGGR_PROC))
      actionLayout.add(new RepaintAction())
      actionLayout.setVisualization(_vis)
      _vis.putAction(ACTION_LAYOUT, actionLayout)
      _vis.alwaysRunAfter(ACTION_COLOR, ACTION_LAYOUT)
      startAnimation()
    }

    def animationStep(): Unit = {
      // requireEDT()
      _vis.synchronized {
        startAnimation()
      }
    }

    def saveFrameAsPNG(file: File): Unit = saveFrameAsPNG(file, width = _dsp.getWidth, height = _dsp.getHeight)

    def saveFrameAsPNG(file: File, width: Int, height: Int): Unit = {
      // requireEDT()
      val bImg  = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      val g     = bImg.createGraphics()
      // val scale = width.toDouble / VIDEO_WIDTH_SQR
      // val p0 = new Point(0, 0)
      try {
        _dsp.damageReport() // force complete redrawing
        // _dsp.zoom(p0, scale)
        // actionAutoZoom.karlHeinz = scale
        val dw = _dsp.getWidth
        val dh = _dsp.getHeight
        val sx = width .toDouble / dw
        val sy = height.toDouble / dh
        g.scale(sx, sy)
        // _dsp.paintDisplay(g, new Dimension(dh, dw))
        _dsp.paintComponent(g)

        // _dsp.zoom(p0, 1.0/scale)
        // actionAutoZoom.karlHeinz = 1.0
        ImageIO.write(bImg, "png", file)
      } finally {
        g.dispose()
      }
    }
  }

  private class Layout(peer: javax.swing.JComponent) extends LayoutManager {
    def layoutContainer(parent: java.awt.Container): Unit =
      peer.setBounds(new Rectangle(0, 0, parent.getWidth, parent.getHeight))

    def minimumLayoutSize  (parent: java.awt.Container): Dimension = peer.getMinimumSize
    def preferredLayoutSize(parent: java.awt.Container): Dimension = peer.getPreferredSize

    def removeLayoutComponent(comp: java.awt.Component): Unit = ()

    def addLayoutComponent(name: String, comp: java.awt.Component): Unit = ()
  }
}
trait Visual {
  def display: Display

  def visualization: Visualization

  def graph: PGraph

  def visualGraph: VisualGraph

  def forceSimulator: ForceSimulator

  def component: Component

  var text: String

  def animationStep(): Unit

  var runAnimation: Boolean

  def saveFrameAsPNG(file: File): Unit

  def saveFrameAsPNG(file: File, width: Int, height: Int): Unit

  var forceParameters: Map[String, Map[String, Float]]

  def layoutCounter: Int

  var autoZoom: Boolean

  var lineWidth : Int

  def setSeed(n: Long): Unit

  var displaySize: Dimension
  var imageSize  : Dimension
}
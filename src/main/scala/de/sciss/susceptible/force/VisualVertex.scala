/*
 *  VisualVertex.scala
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

import prefuse.data.{Node => PNode}
import prefuse.visual.VisualItem

import scala.swing.Graphics2D

object VisualVertex {
  def apply(main: Visual, lineRef: AnyRef, wordRef: AnyRef, character: Char): VisualVertex =
    new Impl(main, lineRef = lineRef, wordRef = wordRef, character = character)

  private final class Impl(val main: Visual, var lineRef: AnyRef, val wordRef: AnyRef, val character: Char)
    extends VisualVertex with VisualVertexImpl {

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit = {
      drawLabel(g, vi, /* diam * vi.getSize.toFloat * 0.5f, */ name)
    }

    protected def boundsResized(): Unit = ()

    def name: String = character.toString

    private var _pNode: PNode = _

    def pNode: PNode = _pNode

    def init(): Unit = {
      _pNode = mkPNode()
      // println(s"ADD NODE ${_pNode.hashCode().toHexString} - $character")
    }

    def dispose(): Unit = {
      if (_pNode.isValid) {
        // println(s"REMOVE NODE ${_pNode.hashCode().toHexString} - $character")
        main.graph.removeNode(_pNode)
      }
      // else println(s"NOT VALID: ${_pNode.hashCode().toHexString}")
    }

    init()
  }
}
sealed trait VisualVertex extends VisualNode {
  def character: Char

  def advance: Int

  def pNode: PNode

  def wordRef: AnyRef
  var lineRef: AnyRef
}
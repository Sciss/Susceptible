/*
 *  GlyphSimilarity.scala
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

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Graphics2D}

final class GlyphSimilarity(font: Font) {
  private[this] var _g : Graphics2D    = _
  private[this] var img: BufferedImage = _
  
  def dispose(): Unit = synchronized {
    if (_g != null) {
      _g.dispose()
      _g = null
    }
    if (img != null) {
      img.flush()
      img = null
    }
  }
  
  private def graphics(): Graphics2D = {
    if (_g == null) {
      if (img == null) {
        img = new BufferedImage(1, 1, BufferedImage.TYPE_BYTE_GRAY)
      }
      _g = img.createGraphics()
      _g.setFont(font)
    }
    _g
  }

  def compare(a: String, b: String): Double = synchronized {
    val g       = graphics()
    val frc     = g.getFontRenderContext
    val vecA    = font.createGlyphVector(frc, a)
    val vecB    = font.createGlyphVector(frc, b)
    val rA      = vecA.getPixelBounds(frc, 0f, 0f)
    val rB      = vecB.getPixelBounds(frc, 0f, 0f)
    val outA    = vecA.getOutline
    val outB    = vecB.getOutline
    import math._
    val minX    = min(rA.x, rB.x)
    val minY    = min(rA.y, rB.y)
    val width   = max(rA.x + rA.width , rB.x + rB.width ) - minX
    val height  = max(rA.y + rA.height, rB.y + rB.height) - minY
    val g2      = if (img.getWidth >= width && img.getHeight >= height * 2) g else {
      dispose()
      img = new BufferedImage(width * 3/2, height * 3, BufferedImage.TYPE_BYTE_GRAY)
      img.createGraphics()
    }

    g2.setColor(Color.black)
    g2.fillRect(0, 0, width, height * 2)
    g2.setColor(Color.white)
    val atOrig    = g2.getTransform
    val clipOrig  = g2.getClip
    // note: despite getPixelBounds promising to give us
    // the bounds that obeyed, the actual string drawing
    // shoots past this. in order to get a 100% correlation
    // between two equal words, we thus clip the drawing area.

    //    g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    g2.clipRect(0, 0, width, height)
    g2.translate(-minX, -minY)
    g2.draw(outA)
    g2.setTransform(atOrig)
    g2.setClip(clipOrig)
    g2.clipRect(0, height, width, height)
    g2.translate(-minX, -minY + height)
    g2.draw(outB)
    g2.setTransform(atOrig)
    g2.setClip(clipOrig)

    var x = 0
    var countA = 0
    var countB = 0
    var countX = 0
    while (x < width) {
      var yA = 0
      var yB = height
      while (yA < height) {
        val hasA = img.getRaster.getSample(x, yA, 0) > 0x7F
        val hasB = img.getRaster.getSample(x, yB, 0) > 0x7F
        if (hasA)         countA += 1
        if (hasB)         countB += 1
        if (hasA & hasB)  countX += 1
        yA += 1
        yB += 1
      }
      x += 1
    }

    val countS = countA + countB
    if (countS == 0) 0.0 else countX * 2.0 / countS
  }
}

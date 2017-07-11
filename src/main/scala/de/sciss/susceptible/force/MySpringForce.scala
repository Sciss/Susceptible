/*
 *  MySpringForce.scala
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

import java.util.Random

import prefuse.util.force.{Spring, SpringForce}

object MySpringForce {
  private final val pi    = math.Pi.toFloat
  private final val piH   = (math.Pi/2).toFloat
  private final val eps   = (0.01 * math.Pi/180).toFloat
}
class MySpringForce extends SpringForce {
  import MySpringForce._

  private val HTORQUE       = params.length
  private val VTORQUE       = HTORQUE + 1
  private val DISTANCE      = VTORQUE + 1
  private val VSPRING_COEFF = DISTANCE + 1
  private val VSPRING_LENGTH = VSPRING_COEFF + 1

  private val rand = new Random(11223344L)

  def setSeed(n: Long): Unit = rand.setSeed(n)

  //                                    htorque vtorque distance vspring   vlength
  params    = params    ++ Array[Float](5e-5f , 5e-5f , -1f    , 8.0e-5f,  100f)
  minValues = minValues ++ Array[Float](0f    , 0f    , -1f    , 0f     ,    1f)
  maxValues = maxValues ++ Array[Float](5e-3f , 5e-3f , 500f   , 8.0e-3f, 1000f)

  override def getParameterNames: Array[String] =
    super.getParameterNames ++ Array("HTorque", "VTorque", "Limit", "VSpring", "VLength")

  // https://stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
  private def angleBetween(a: Float, b: Float): Float = {
    val d = b - a
    math.atan2(math.sin(d), math.cos(d)).toFloat
  }

  override def getForce(s: Spring): Unit = {
    val isHorizontal = s.coeff == 0f || s.coeff == 1f

    val item1   = s.item1
    val item2   = s.item2
    val length  = if (isHorizontal) s.length /* params(SpringForce.SPRING_LENGTH) */ else params(VSPRING_LENGTH)
    val x1      = item1.location(0)
    val y1      = item1.location(1)
    val x2      = item2.location(0)
    val y2      = item2.location(1)
    var dx      = x2 - x1
    var dy      = y2 - y1
    val r0      = math.sqrt(dx * dx + dy * dy).toFloat
    val r1      = if (r0 == 0.0) {
      dx  = (rand.nextFloat() /* math.random.toFloat */ - 0.5f) / 50.0f
      dy  = (rand.nextFloat() /* math.random.toFloat */ - 0.5f) / 50.0f
      math.sqrt(dx * dx + dy * dy).toFloat
    } else r0
    val dist    = params(DISTANCE)
    val r       = if (dist < 0) r1 else math.min(dist, r1)

    val d       = r - length

    // val coeff   = (if (s.coeff < 0) params(SpringForce.SPRING_COEFF) else s.coeff) * d / r
    val coeff = if (isHorizontal) params(SpringForce.SPRING_COEFF) else params(VSPRING_COEFF)
    val amt   = coeff * d / r

    item1.force(0) +=  amt * dx
    item1.force(1) +=  amt * dy
    item2.force(0) += -amt * dx
    item2.force(1) += -amt * dy

    val torqueAngle = if (isHorizontal) 0f else piH
    val torque      = if (isHorizontal) params(HTORQUE) else params(VTORQUE)

    val ang = math.atan2(dy, dx).toFloat
    val da = angleBetween(ang, torqueAngle)
    if (math.abs(da) > eps) {
      val af  = da / pi * torque
      // println(f"ang = $ang%1.3f, da = $da%1.3f, af = $af%1.3f")
      val rH  = r/2
      val cx  = (x1 + x2) / 2
      val cy  = (y1 + y2) / 2
      val cos = math.cos(ang + af).toFloat * rH
      val sin = math.sin(ang + af).toFloat * rH
      val x1t = cx - cos
      val y1t = cy - sin
      val x2t = cx + cos
      val y2t = cy + sin

      item1.force(0) += x1t - x1
      item1.force(1) += y1t - y1
      item2.force(0) += x2t - x2
      item2.force(1) += y2t - y2
    }
  }
}
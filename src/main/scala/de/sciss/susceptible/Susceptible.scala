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

import java.io.FileInputStream

import de.sciss.file._

object Susceptible {
  final case class Config(textFile: File)

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

  def run(config: Config): Unit = {

  }
}
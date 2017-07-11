package de.sciss.susceptible

object MSTTest extends App {
  val edges = List(
    Edge("A", "B",  6.0),
    Edge("A", "C",  3.0),
    Edge("A", "D",  9.0),
    Edge("B", "C",  4.0),
    Edge("B", "E",  9.0),
    Edge("B", "F",  2.0),
    Edge("C", "D",  9.0),
    Edge("C", "F",  2.0),
    Edge("C", "G",  9.0),
    Edge("D", "G",  8.0),
    Edge("D", "H", 18.0),
    Edge("E", "F",  9.0),
    Edge("E", "G",  7.0),
    Edge("E", "I",  4.0),
    Edge("E", "J",  5.0),
    Edge("F", "G",  8.0),
    Edge("G", "H", 10.0),
    Edge("G", "J",  9.0),
    Edge("H", "I",  4.0),
    Edge("H", "J",  3.0),
    Edge("I", "J",  1.0)
  )

  val res = MSTKruskal[String, Edge[String]](edges)
  println(s"MST size = ${res.size}\n")
  res.foreach(println)
}

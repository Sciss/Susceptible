package de.sciss.susceptible

trait EdgeLike[+A] {
  def start : A
  def end   : A
  def weight: Double
}

final case class Edge[+A](start: A, end: A, weight: Double) extends EdgeLike[A] {
  override def toString: String = s"$productPrefix(start = $start, end = $end, weight = $weight)"
}

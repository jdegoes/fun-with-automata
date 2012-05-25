
object Analytics {
  import DT.{identity, monoid, rep3, count}
  
  val id = identity[Double]
  
  val sum = monoid[Double](0.0, _ + _)
  
  val product = monoid[Double](1.0, _ * _)
  
  val square = id.map(v => v * v)
  
  val stdDev = (rep3[Double] >>> (count & sum & square)).map {
    case ((s0, s1), s2) => math.sqrt((s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0)))
  }
}
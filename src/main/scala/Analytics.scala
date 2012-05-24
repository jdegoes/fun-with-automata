
object Analytics {
  private def monoid(zero: Double, append: (Double, Double) => Double) = new DT[Double, Double] {
    def ! (v: Double) = {
      def recurse(acc: Double): DT[Double, Double] = new DT[Double, Double] {
        def ! (v: Double): (Double, DT[Double, Double]) = {
          val total = append(v, acc)
          (total, recurse(total))
        }
      }
    
      recurse(zero) ! v
    }
  }
  
  def count[A] = {
    def recurse(n: Int): DT[A, Int] = DT { v: A =>
      (n, recurse(n + 1))
    }
    
    recurse(1)
  }
  
  val id = DT.identity[Double]
  
  val sum = monoid(0.0, _ + _)
  
  val product = monoid(1.0, _ * _)
  
  val square = id.map(v => v * v)
  
  val stdDev = (DT.rep3 >>> (id & id & id) >>> (count & sum & square)).map {
    case ((s0, s1), s2) => math.sqrt((s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0)))
  }
  
  def select(n: Int): DT[Double, Vector[Double]] = {
    def recurse(acc: Vector[Double]): DT[Double, Vector[Double]] = {
      if (acc.length < n) DT { v: Double =>
        val nextAcc = acc :+ v
        
        (nextAcc, recurse(nextAcc))
      } else DT { v: Double =>
        val nextAcc = acc.drop(1)  :+ v
        
        (nextAcc, recurse(nextAcc))
      }
    }
    
    recurse(Vector.empty)
  }
}
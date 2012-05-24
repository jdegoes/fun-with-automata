import scala.annotation.tailrec

trait DT[A, B] { self =>
  
  def ! (input: A): (B, DT[A, B])
  
  def !! (inputs: A*): (List[B], DT[A, B]) = {
    @tailrec def recurse(inputs: Seq[A], acc: List[B], self: DT[A, B]): (List[B], DT[A, B]) = inputs.headOption match {
      case None => (acc.reverse, self)
      
      case Some(head) =>
        val (b, self2) = self ! head
        
        recurse(inputs.tail, b :: acc, self2)
    }
    
    recurse(inputs, Nil, self)
  }

  def >>> [C] (that: DT[B, C]): DT[A, C] = new DT[A, C] {
    def ! (input: A): (C, DT[A, C]) = {
      val (b, self2) = self ! input
      val (c, that2) = that ! b
      
      (c, self2 >>> that2)
    }
  }

  def map[C](f: B => C): DT[A, C] = new DT[A, C] {
    def ! (input: A): (C, DT[A, C]) = {
      val (b, self2) = self ! input
      
      (f(b), self2.map(f))
    }
  }

  def contramap[C](f: C => A): DT[C, B] = new DT[C, B] {
    def ! (input: C): (B, DT[C, B]) = {
      val (b, self2) = self ! f(input)
      
      (b, self2.contramap(f))
    }
  }
  
  def & [C, D](that: DT[C, D]): DT[(A, C), (B, D)] = new DT[(A, C), (B, D)] {
    def ! (v: (A, C)): ((B, D), DT[(A, C), (B, D)]) = {
      val (b, self2) = self ! v._1
      val (d, that2) = that ! v._2
      
      ((b, d), self2 & that2)
    }
  }
}

object DT {
  def apply[A, B](f: A => (B, DT[A, B])) =
    new DT[A, B] { def ! (v: A) = f(v) }
    
  def identity[A]: DT[A, A] = {
    lazy val self: DT[A, A] = apply { v: A =>
      (v, self)
    }
    
    self
  }
  
  def constant[A, B](b: B): DT[A, B] = identity[A].map(v => b)

  def rep[A]: DT[A, (A, A)] = identity[A].map(v => (v, v))
  
  def rep3[A]: DT[A, ((A, A), A)] = identity[A].map(v => ((v, v), v))
  
  def rep4[A]: DT[A, (((A, A), A), A)] = identity[A].map(v => (((v, v), v), v))

  def merge[A, B, C](f: (A, B) => C): DT[(A, B), C] = {
    lazy val self: DT[(A, B), C] = apply { v: (A, B) =>
      (f(v._1, v._2), self)
    }
    
    self
  }
  
  def merge3[A, B, C, D](f: (A, B, C) => D): DT[((A, B), C), D] = {
    lazy val self: DT[((A, B), C), D] = apply { v: ((A, B), C) =>
      (f(v._1._1, v._1._2, v._2), self)
    }
    
    self
  }
}
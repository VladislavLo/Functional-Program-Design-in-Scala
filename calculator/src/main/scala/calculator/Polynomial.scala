package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b()

      (bVal * bVal) - 4 * (a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set()
      else {
        val negativeB = Signal(-1 * b())
        val sqrt = Signal(math.sqrt(delta()))
        val twoA = Signal(2 * a())
        Set(
          (negativeB() + sqrt()) / twoA(),
          (negativeB() - sqrt()) / twoA())
      }
    }
  }
}

package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal((b() * b()) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {

      var setResults: Set[Double] = Set()

      if (computeDelta(a, b, c)() > 0) {
        setResults += (-1 * b() + Math.sqrt(delta())) / (2 * a())
        setResults += (-1 * b() - Math.sqrt(delta())) / (2 * a())
      } else if (computeDelta(a, b, c)() == 0) {
        setResults += ((-1 * b()) / (2 * a()))
      }

      setResults
    }
  }
}

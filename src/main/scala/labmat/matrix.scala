package labmat

/** A base type for row- and column-vectors of double. */
sealed trait Vec {
  def toVector: Vector[Double]

  def apply(i: Int): Double = toVector(i)
  
  def toRow: Row = Row(toVector)
  
  def toCol: Col = Col(toVector)
  
  def size: Int = toVector.length
  
  def *(that: Vec): Vec = ???

  def isCol: Boolean = this.isInstanceOf[Col]
  def isRow: Boolean = this.isInstanceOf[Row]
  
  def toMat: Mat = this match {
    case r: Row => Mat(r)
    case c: Col => Mat(c)
  }
}

object Vec {
  def apply(xs: Double*): Vec = Row(xs.toVector)
}

/** A column vector of doubles. */
case class Col(toVector: Vector[Double]) extends Vec {
  override def toString = toVector.mkString("Col(\n  ",",\n  ","\n)")
}

object Col {
  def apply(xs: Double*): Col = Col(xs.toVector)
}

/** A row vector of doubles. */
case class Row(toVector: Vector[Double]) extends Vec{
  override def toString = toVector.mkString("Row(",", ",")")
}
object Row {
  def apply(xs: Double*): Row = Row(xs.toVector)
  def apply(s: String): Row = Row(s.split(" ").map(_.toDouble).toVector)
}

/** A matrix of doubles.*/
case class Mat(toVector: Vector[Double], dim: (Int, Int)){
  val nRow = dim._1
  val nCol = dim._2

  require(toVector.length == nRow * nCol, s"underlying vector must fit dim $dim")
  require(nCol >= 0 && nRow >= 0, s"dim must be positive")

  val isSquare: Boolean = nRow == nCol 

  val rowInd: Range = 0 until nRow
  val colInd: Range = 0 until nCol

  /** Gives the value of the matrix at row r, column c */
  def apply(r: Int, c: Int): Double = toVector(r * nCol + c)

  def row(r: Int): Row = Row(toVector.slice(r * nCol, r * nCol + nCol))
  def col(c: Int): Col = Col((for (r <- rowInd) yield toVector(r * nRow + c)).toVector)

  lazy val rows: Vector[Row] = (for (r <- rowInd) yield row(r)).toVector
  lazy val cols: Vector[Col] = (for (c <- colInd) yield col(c)).toVector

  val isEmpty: Boolean = dim == (0,0)
  val nonEmpty: Boolean = !isEmpty

  def transpose: Mat = Mat(cols: _*)

  def map(f: Double => Double): Mat = Mat(toVector.map(f), dim)

  override def toString = rows.mkString("Mat(\n  ",",\n  ","\n)")

  def show: String = rows.map(_.toVector.mkString(" ")).mkString("\n")
}

object Mat {
  def empty: Mat = Mat(Vector.empty[Double], (0, 0))

  def apply(vs: Vec*): Mat = if (vs.size == 0) Mat.empty else {
    if (vs(0).isRow) {
      require(vs.forall(_.isRow), s"all vectors must be of same type: $vs")
      require(vs.forall(_.size == vs(0).size), s"all must be of same size")
      Mat(vs.toVector.map(_.toVector).flatten, (vs.size, vs(0).size))
    } else {
      require(vs.forall(_.isCol), s"all vectors must be of same type: $vs")
      require(vs.forall(_.size == vs(0).size), "all must be of same size")
      val nRows = vs(0).size 
      val nCols = vs.size
      val xs: Seq[Double] = 
        for (r <- 0 until nRows; c <- 0 until nCols)  yield vs(c)(r)
      Mat(xs.toVector, (nRows, nCols))
    }
  }

  def apply(s: String): Mat = ???

  def apply(nRows: Int, nCols: Int)(xs: Double*): Mat = Mat(xs.toVector, (nRows, nCols))

  def fill(r: Int, c: Int)(value: Double): Mat = Mat(Vector.fill(r * c)(value), (r, c))

  def diag(rc: Int)(value: Double): Mat = tabulate(rc, rc)((r, c) => if (r == c) value else 0.0)

  def zeros(r: Int, c: Int): Mat = fill(r, c)(0.0)

  def ones(r: Int, c: Int): Mat = fill(r, c)(1.0)

  def tabulate(r: Int, c: Int)(f: (Int, Int) => Double): Mat = 
    Mat((for (x <- 0 until r; y <- 0 until c) yield f(x, y)).toVector, (r, c))

  def unit(rc: Int): Mat = diag(rc)(1.0)

  def square(rc: Int)(value: Double): Mat = fill(rc,rc)(value)

  
}
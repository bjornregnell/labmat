package labmat

trait IO {
    def loadMat(filename: String): Mat = {
      val lines = introprog.IO.loadLines(filename)
      val rs: Vector[Row] = lines.map(Row.apply)
      val nRows = rs.size
      val nCols = scala.util.Try(rs(0).size).getOrElse(0)
      Mat(rs: _*)
    }
    
    def saveMat(filename: String, m: Mat): Unit = introprog.IO.saveString(m.show, fileName=filename)

    /** Extension methods for Mat, so you can write m.save("file.mat") */
    implicit class MatOps(m: Mat) {
      def save(filename: String): Unit = saveMat(filename, m)
    }

    /** Extension methods companion Mat, so you can write Mat.load("file.mat") */
    implicit class MatCompanionOps(obj: Mat.type) {
      def load(filename: String) = loadMat(filename)
    }
}
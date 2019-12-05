package labmat

trait Plot {
  import introprog.PixelWindow

  def plot(
    m: Mat, 
    windowWidth: Int = 200, 
    windowHeight: Int = 200,
    foreground: java.awt.Color = java.awt.Color.WHITE,
    background: java.awt.Color = java.awt.Color.BLACK,
    lineColor:  java.awt.Color = java.awt.Color.BLACK
  ): PixelWindow = {
    require(m.nCols == 2, "matrix must be 2 cols with x values in first col")
    val w = new introprog.PixelWindow(
      width = windowWidth, height = windowWidth, 
      title = "Matrix plot",
      foreground = foreground,
      background = background,
    )
    ??? 
    /* calculate min max values */
    /* drawAxes */  
    /* plot function from col(0) on x-axis and col(1) on y-axis */

    w
  }
}
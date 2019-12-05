package labmat

trait Plot {
  import introprog.PixelWindow


  def plot(f: Double => Double)(xStart: Double, xEnd: Double): PixelWindow = {
    val w = new PixelWindow
    w
  } 
}
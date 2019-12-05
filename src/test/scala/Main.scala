import scala.util.Success
import scala.util.Failure
object Main {
  var passed =  Vector.empty[String] 
  var failed =  Vector.empty[String] 

  def shouldBeTrue(name: String)(b: => Boolean): Unit = scala.util.Try(b) match {
      case Success(true)      => passed :+= s"$name => true"
      case Success(false)     => failed :+= s"$name => false"
      case Failure(exception) => failed :+= s"$name => $exception"      
    }

  def shouldSucceed[T](name: String)(b: => T): Unit = scala.util.Try(b) match {
    case Success(value)     => passed :+= s"$name => $value" 
    case Failure(exception) => failed :+= s"$name => $exception"      
  }

  def shouldFail[T](name: String)(b: => T): Unit = scala.util.Try(b) match {
    case Success(value)     => failed :+= s"$name => $value"
    case Failure(exception) => passed :+= s"$name => $exception"       
  }

  def report(): Unit = {
    println("----------------- TESTING ---------------------")
    println(s"PASSED: ${passed.length}${passed.mkString("\n  ","\n  ", "\n")}")
    println(s"FAILED: ${failed.length}${failed.mkString("\n  ","\n  ", "\n")}")
    println("-----------------------------------------------")
    assert(failed.isEmpty, s"FAILED: ${failed.length}")
  }

  def main(args: Array[String]): Unit = {
    import labmat._

    shouldBeTrue("contruct Mat from Row"){ 
      Mat(Row(1,2,3), Row(1,2,3)) == Mat(Vector[Double](1,2,3,1,2,3), (2,3)) 
    }
    
    shouldBeTrue("contruct Mat from Col"){ 
      Mat(Col(1,2,3), Col(1,2,3)) == Mat(Vector[Double](1,1,2,2,3,3), (3,2)) 
    }
    
    shouldFail("div one by zero") { 1 / 0 }
    
    shouldSucceed("div one by one") { 1 / 1 }

    report()
  }
}
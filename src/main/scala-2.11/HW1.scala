import breeze.linalg.{DenseVector, DenseMatrix}
import scala.util.matching.Regex

/**
 * Created by roackb2 on 15/5/28.
 */
object HW1 extends App {


    def getModel: PLA = {
        import PLA.nestedListToMatrix
        val url = "https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat"
        lazy val data: DenseMatrix[Double] = {
            println("getting data")
            scala.io.Source.fromURL(url).getLines.toList.map(line => line.split("\\s").toList.map(_.toDouble))
        }
        val X = data(::, 0 to data.cols - 2)
        val Y = data(::, -1).map(x => if(x == 1) true else false)
        PLA(X, Y)
    }

    val pla = getModel
    println(pla)
}

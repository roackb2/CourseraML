import breeze.linalg.{DenseMatrix}
import PLA._

/**
 * Created by roackb2 on 15/5/28.
 */
object HW1 extends App {


    def getData(url: String): DenseMatrix[Double] = {
        lazy val source: DenseMatrix[Double] = {
            println("getting data")
            scala.io.Source.fromURL(url)
        }
        source
    }


    def Q15(m: DenseMatrix[Double]): Int = {
        PLA(m).train
    }

    def Q16(m: DenseMatrix[Double]): Double = {
        var counter = 0
        val counts: List[Int] = (1 to 2000).par.map(x => {
            counter += 1
            println("finding update count for No." + counter + " sequence")
            Q15(PLA.shuffleRow(m))
        }).toList
        println(counts)
        counts.sum / counts.length
    }

    def Q18(trainingSet: DenseMatrix[Double], testSet: DenseMatrix[Double]): Double = {
        var counter = 0
        val rates: List[Double] = (1 to 2000).par.map(x => {
            counter += 1
            println("finding error rate for No." + counter + " sequence")
            val trainingPla = PLA(shuffleRow(trainingSet))
            val verifyingPla = PLA(testSet)
            trainingPla.train(50)
            val correctness = PLA(verifyingPla.X, verifyingPla.Y)(trainingPla.pocket).verify
            correctness.count(x => !x).toDouble / correctness.length
        }).toList
        rates.sum / rates.length
    }

    def Q19(trainingSet: DenseMatrix[Double], testSet: DenseMatrix[Double]): Double = {
        var counter = 0
        val rates: List[Double] = (1 to 2000).par.map(x => {
            counter += 1
            println("finding error rate for No." + counter + " sequence")
            val trainingPla = PLA(shuffleRow(trainingSet))
            val verifyingPla = PLA(testSet)
            trainingPla.train(50)
            val correctness = PLA(verifyingPla.X, verifyingPla.Y)(trainingPla.w).verify
            correctness.count(x => !x).toDouble / correctness.length
        }).toList
        rates.sum / rates.length
    }

    def Q20(trainingSet: DenseMatrix[Double], testSet: DenseMatrix[Double]): Double = {
        var counter = 0
        val rates: List[Double] = (1 to 2000).par.map(x => {
            counter += 1
            println("finding error rate for No." + counter + " sequence")
            val trainingPla = PLA(shuffleRow(trainingSet))
            val verifyingPla = PLA(testSet)
            trainingPla.train(100)
            val correctness = PLA(verifyingPla.X, verifyingPla.Y)(trainingPla.pocket).verify
            correctness.count(x => !x).toDouble / correctness.length
        }).toList
        rates.sum / rates.length
    }

    val data: DenseMatrix[Double] = getData("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")
    println("done getting data")
//    val trainingSet: DenseMatrix[Double] = getData("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_train.dat")
//    val testSet: DenseMatrix[Double] = getData("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_test.dat")


    println(Q15(data(1 to 5, ::)))
//    println(Q16(data))
//    println(Q18(trainingSet, testSet))
//    println(Q19(trainingSet, testSet))
//    println(Q20(trainingSet, testSet))

}

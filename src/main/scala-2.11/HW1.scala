import breeze.linalg.{DenseMatrix}
import PLA._

/**
 * Created by roackb2 on 15/5/28.
 */
object HW1 extends App {

    def getData(url: String): DenseMatrix[Double] = {
        lazy val source: DenseMatrix[Double] = {
            println("getting data from " + url)
            scala.io.Source.fromURL(url)
        }
        source
    }
    
    def Q15(m: DenseMatrix[Double]): Int = {
        PLA(m).train()
    }

    def Q16(m: DenseMatrix[Double], showProgress: Boolean = true): Double = {
        var counter = 0
        val counts: List[Int] = (1 to 2000).par.map(x => {
            counter += 1
            if(showProgress) println("finding update count for No." + counter + " sequence")
            PLA(shuffleRow(m)).train()
        }).toList
        counts.sum.toDouble / counts.length.toDouble
    }

    def Q17(m: DenseMatrix[Double], showProgress: Boolean = true): Double = {
        var counter = 0
        val counts: List[Int] = (1 to 2000).par.map(x => {
            counter += 1
            if(showProgress) println("finding update count for No." + counter + " sequence")
            PLA(shuffleRow(m)).train(stepSize = 0.5)
        }).toList
        counts.sum.toDouble / counts.length.toDouble
    }

    def Q18(trainingSet: DenseMatrix[Double], testSet: DenseMatrix[Double], showProgress: Boolean = true): Double = {
        var counter = 0
        val rates: List[Double] = (1 to 2000).par.map(x => {
            counter += 1
            if(showProgress) println("finding error rate for No." + counter + " sequence")
            val trainingPla = PLA(shuffleRow(trainingSet))
            val verifyingPla = PLA(testSet)
            trainingPla.train(50)
            val correctness = verifyingPla.verify(trainingPla.pocket)
            correctness.count(x => !x).toDouble / correctness.length.toDouble
        }).toList
        rates.sum / rates.length.toDouble
    }

    def Q19(trainingSet: DenseMatrix[Double], testSet: DenseMatrix[Double], showProgress: Boolean = true): Double = {
        var counter = 0
        val rates: List[Double] = (1 to 2000).par.map(x => {
            counter += 1
            if(showProgress) println("finding error rate for No." + counter + " sequence")
            val trainingPla = PLA(shuffleRow(trainingSet))
            val verifyingPla = PLA(testSet)
            trainingPla.train(50)
            val correctness = verifyingPla.verify(trainingPla.w)
            correctness.count(x => !x).toDouble / correctness.length.toDouble
        }).toList
        rates.sum / rates.length.toDouble
    }

    def Q20(trainingSet: DenseMatrix[Double], testSet: DenseMatrix[Double], showProgress: Boolean = true): Double = {
        var counter = 0
        val rates: List[Double] = (1 to 2000).par.map(x => {
            counter += 1
            if(showProgress) println("finding error rate for No." + counter + " sequence")
            val trainingPla = PLA(shuffleRow(trainingSet))
            val verifyingPla = PLA(testSet)
            trainingPla.train(100)
            val correctness = verifyingPla.verify(trainingPla.pocket)
            correctness.count(x => !x).toDouble / correctness.length.toDouble
        }).toList
        rates.sum / rates.length.toDouble
    }

    val data: DenseMatrix[Double] = getData("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")
    val trainingSet: DenseMatrix[Double] = getData("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_train.dat")
    val testSet: DenseMatrix[Double] = getData("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_test.dat")

    println("Q15: " + Q15(data))
    println("Q16: " + Q16(data, false))
    println("Q17: " + Q17(data, false))
    println("Q18: " + Q18(trainingSet, testSet, false))
    println("Q19: " + Q19(trainingSet, testSet, false))
    println("Q20: " + Q20(trainingSet, testSet, false))
}

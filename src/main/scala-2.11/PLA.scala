import breeze.linalg._
import PLA._
import scala.io.BufferedSource

/**
 * Created by roackb2 on 15/5/26.
 */
case class PLA(input: DenseMatrix[Double], label: DenseVector[Boolean])(implicit init: DenseVector[Double] = DenseVector.zeros[Double](input.cols + 1)) {

    val X: DenseMatrix[Double] = DenseMatrix.horzcat(DenseMatrix.zeros[Double](input.rows, 1) :+ 1.0, input)
    val Y: DenseVector[Boolean] = label.copy
    val w: DenseVector[Double] = init.copy
    val pocket: DenseVector[Double] = init.copy

    /**
     * The data matrix contains all input and labels, each row is an input with its label as the last element,
     * positive numbers are true and negative ones are false
     */
    def this(data: DenseMatrix[Double]) = this(data(::, 0 to -2), data(::, -1).map(x => if(x > 0) true else false))

    override def toString: String = {
        (X: List[DenseVector[Double]]).zip(Y).foldLeft("")((former, pair) => pair match {
            case (row, y) => former + row.asDenseMatrix +  y + "\n"
        })
    }

    def add(x: DenseVector[Double], y: Boolean): PLA = {
        PLA(DenseMatrix.vertcat[Double](input, x.asDenseMatrix), DenseVector.vertcat[Boolean](label, DenseVector(y)))
    }

    def addAll(x: DenseMatrix[Double], y: DenseVector[Boolean]): PLA = {
        PLA(DenseMatrix.vertcat(input, x), DenseVector.vertcat(label, y))
    }

    def predict(hypothesis: DenseVector[Double] = w): DenseVector[Double] = {
        X * hypothesis
    }

    def verify(hypothesis: DenseVector[Double] = w): DenseVector[Boolean] = {
        predict(hypothesis).map(x => x > 0) :== Y
    }

    def findMistake(lastIndex: Int, correctness: DenseVector[Boolean])(implicit random: Boolean = false): Int = {
        val mistakeIndices: List[Int] = correctness.findAll(x => !x).toList
        if(random) util.Random.shuffle(mistakeIndices).head else
        mistakeIndices.find(x => x > lastIndex) match {
            case Some(n) => n
            case None => mistakeIndices.head
        }
    }

    def update(index: Int, stepSize: Double = 1.0): DenseVector[Double] = {
        w + (X(index, ::).t :* (if(Y(index)) stepSize else -stepSize))
    }

    /* if bound < 0 then run without pocket algorithm */
    def train(bound: Int = -1, randomVisit: Boolean = false, stepSize: Double = 1.0): Int = {
        w := DenseVector.zeros[Double](X.cols)
        pocket := DenseVector.zeros[Double](X.cols)
        val correctness: DenseVector[Boolean] = verify()
        var counter = 0
        var lastIndex = 0
        while(!correctness.forall(y => y) && (if(bound > 0) counter < bound else true)) {
            counter += 1
            val index:Int = findMistake(lastIndex, correctness)(randomVisit)
            lastIndex = index
            w := update(index, stepSize)
            correctness := verify()
            if(bound > 0 && correctness.count(x => x) > verify(pocket).count(x => x)) pocket := w
        }
        counter
    }

    def printStatus(index: Int): Unit = {
        println("X:")
        println(X)
        println("Y:")
        println(Y.asDenseMatrix)
        println("index: " + index)
        println("w:")
        println(w.asDenseMatrix)
        println("prediction:")
        println(predict().asDenseMatrix)
        println("correctness:")
        println(verify().asDenseMatrix)
    }

}

object PLA {

//    def apply(X: DenseMatrix[Double], Y: DenseVector[Boolean])(implicit init: DenseVector[Double] = DenseVector.zeros[Double](X.cols)) = {}

    def apply(data: DenseMatrix[Double]): PLA = {
        new PLA(data)
    }

    def shuffleRow(m: DenseMatrix[Double]): DenseMatrix[Double] = {
        val result: List[List[Double]] = util.Random.shuffle(m)
        result
    }

    implicit def intVectorToDouble(v: DenseVector[Int]): DenseVector[Double] = {
        v.map(x => x.toDouble)
    }

    implicit def intMatrixToDouble(m: DenseMatrix[Int]): DenseMatrix[Double] = {
        m.map(x => x.toDouble)
    }

    implicit def matrixToVectorList(m :DenseMatrix[Double]): List[DenseVector[Double]] = {
        List.tabulate[DenseVector[Double]](m.rows)(n => m(n, ::).t)
    }

    implicit def matrixToNestedList(m: DenseMatrix[Double]): List[List[Double]] = {
        List.tabulate[List[Double]](m.rows)(x => List.tabulate[Double](m.cols)(y => m(x, y)))
    }

    implicit def nestedListToMatrix(list: List[List[Double]]): DenseMatrix[Double] = {
        DenseMatrix(list.map(_.toArray):_*)
        /* List:_* means applly elements in the list to a method as arbitrary number of arguments (varargs) */
    }

    /**
     * Input source that is text in format of that,
     * each row contains attributes, and label is the last element(positive and negative numbers,
     * typically 1.0 or -1.0),
     * separated by space.
     */
    implicit def sourceToMatrix(source: BufferedSource): DenseMatrix[Double] = {
        source.getLines.toList.map(line => line.split("\\s").toList.map(_.toDouble))
    }

    implicit def doubleVectorToList(v: DenseVector[Double]): List[Double] = {
        v.toArray.toList
    }

    implicit def booleanVectorToList(v: DenseVector[Boolean]): List[Boolean] = {
        v.toArray.toList
    }


}

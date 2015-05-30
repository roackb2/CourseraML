import breeze.linalg._
import PLA._
import scala.io.BufferedSource

/**
 * Created by roackb2 on 15/5/26.
 */
case class PLA(X: DenseMatrix[Double], Y: DenseVector[Boolean])(implicit init: DenseVector[Double] = DenseVector.zeros[Double](X.cols)) {

    val w: DenseVector[Double] = init
    val pocket: DenseVector[Double] = init

    /**
     * The data matrix contains all input and labels, each row is an input with its label as the last element,
     * positive numbers are true and negative ones are false
     */
    def this(data: DenseMatrix[Double]) = this(data(::, 0 to -2), data(::, -1).map(x => if(x > 0) true else false))

    override def toString: String = {
        (X: List[DenseVector[Double]]).zip(Y).foldLeft("")((former, pair) => pair match {
            case (row, y) => former + vectorToString(row) +  y + "\n"
        })
    }

    def add(x: DenseVector[Double], y: Boolean): PLA = {
        PLA(DenseMatrix.vertcat[Double](X, x.asDenseMatrix), DenseVector.vertcat[Boolean](Y, DenseVector(y)))
    }

    def addAll(x: DenseMatrix[Double], y: DenseVector[Boolean]): PLA = {
        PLA(DenseMatrix.vertcat(X, x), DenseVector.vertcat(Y, y))
    }

    def predict(implicit input: DenseMatrix[Double] = X, label: DenseVector[Boolean] = Y, hypothesis: DenseVector[Double] = w): DenseVector[Boolean] = {
        DenseVector((input * hypothesis).toArray.toList.map(x => x > 0).toArray)
    }

    def verify(implicit input: DenseMatrix[Double] = X, label: DenseVector[Boolean] = Y, hypothesis: DenseVector[Double] = w): DenseVector[Boolean] = {
        predict(input, label, hypothesis) :== Y
    }

    def update(index: Int, correctness: DenseVector[Boolean])(implicit input: DenseMatrix[Double] = X,  label: DenseVector[Boolean] = Y, hypothesis: DenseVector[Double] = w): DenseVector[Double] = {
        hypothesis + (input(index, ::).t :*= (if(correctness(index)) 1.0 else -1.0))
    }

    /* if bound < 0 then run without pocket algorithm */
    def train(implicit bound: Int = -1): Int = {
        w := DenseVector.zeros[Double](X.cols)
        pocket := DenseVector.zeros[Double](X.cols)
        var correctness: DenseVector[Boolean] = verify
        var counter = 0
        while(!correctness.forall(y => y) && (if(bound > 0) counter < bound else true)) {
            counter += 1
            val index:Int = correctness.findAll(x => !x).head
            w := update(index, correctness)
            correctness = verify
            if(bound > 0 && correctness.count(x => x) > PLA(X, Y)(pocket).verify.count(x => x)) pocket := w
        }
        counter
    }

}

object PLA {

    def apply(data: DenseMatrix[Double]): PLA = {
        new PLA(data)
    }

    def vectorToString(v: DenseVector[Double]): String = {
        v.foldLeft("")((prefix, x) => prefix + x.toString + " ").toString
    }

    def shuffleRow(m: DenseMatrix[Double]): DenseMatrix[Double] = {
        val result: List[List[Double]] = util.Random.shuffle(m)
        result
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
     * Input source that is in format of that,
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

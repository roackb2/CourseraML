import breeze.linalg._
/**
 * Created by roackb2 on 15/5/26.
 */
case class PLA(X: DenseMatrix[Double], Y: DenseVector[Boolean]) {


    implicit def matrixToList(m :DenseMatrix[Double]): List[DenseVector[Double]] = {
        List.tabulate[DenseVector[Double]](m.rows)(n => m(n, ::).t)
    }

    implicit def doubleVectorToList(v: DenseVector[Double]): List[Double] = {
        v.toArray.toList
    }

    implicit def booleanVectorToList(v: DenseVector[Boolean]): List[Boolean] = {
        v.toArray.toList
    }


    override def toString(): String = {
        X.zip(Y).foldLeft("")((former, pair) => pair match {
            case (row: DenseVector[Double], y: Boolean) => former + PLA.vectorToString(row) + " " +  y + "\n"
        })
    }

    def add(x: DenseVector[Double], y: Boolean): PLA = {
        PLA(DenseMatrix.vertcat[Double](X, x.asDenseMatrix), DenseVector.vertcat[Boolean](Y, DenseVector(y)))
    }

    def addAll(x: DenseMatrix[Double], y: DenseVector[Boolean]): PLA = {
        PLA(DenseMatrix.vertcat(X, x), DenseVector.vertcat(Y, y))
    }

    def predict(w: DenseVector[Double]): DenseVector[Boolean] = {
        DenseVector((X * w).toArray.toList.map(x => x > 0).toArray)
    }

    def train: DenseVector[Double] = {
        var w: DenseVector[Double] = DenseVector.zeros[Double](X.cols)
        var answer: DenseVector[Boolean] = predict(w) :== Y
        while(!answer.forall(y => y)) {
            val index:Int = answer.findAll(x => !x).head
            w += (X(index, ::).t  :*= (if(answer(index)) 1.0 else -1.0))
            answer = predict(w) :== Y
        }
        w
    }
}

object PLA {
    def vectorToString(v: DenseVector[Double]): String = {
        v.foldLeft("")((prefix, x) => prefix + " " + x).toString
    }
}

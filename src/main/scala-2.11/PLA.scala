import breeze.linalg._
/**
 * Created by roackb2 on 15/5/26.
 */
case class PLA(X: DenseMatrix[Double], Y: DenseVector[Boolean])(implicit w: DenseVector[Double] = DenseVector.zeros[Double](X.cols)) {

    import PLA._

    override def toString: String = {
        X.zip(Y).foldLeft("")((former, pair) => pair match {
            case (row, y) => former + vectorToString(row) +  y + "\n"
        })
    }

    def add(x: DenseVector[Double], y: Boolean): PLA = {
        PLA(DenseMatrix.vertcat[Double](X, x.asDenseMatrix), DenseVector.vertcat[Boolean](Y, DenseVector(y)))
    }

    def addAll(x: DenseMatrix[Double], y: DenseVector[Boolean]): PLA = {
        PLA(DenseMatrix.vertcat(X, x), DenseVector.vertcat(Y, y))
    }

    def predict: DenseVector[Boolean] = {
        DenseVector((X * w).toArray.toList.map(x => x > 0).toArray)
    }

    /* if bound < 0 then run without pocket algorithm */
    def train(implicit bound: Int = -1): DenseVector[Double] = {
        var pocket: DenseVector[Double] = w
        var answer: DenseVector[Boolean] = predict :== Y
        var counter = 0
        while(!answer.forall(y => y) && (if(bound > 0) counter < bound else true)) {
            counter += 1
            val index:Int = answer.findAll(x => !x).head
            w += (X(index, ::).t  :*= (if(answer(index)) 1.0 else -1.0))
            answer = predict :== Y
            if(bound > 0 && answer.count(x => x) > (PLA(X, Y)(pocket).predict :== Y).count(x => x)) {
                pocket = w
            }
        }
        if(bound > 0) pocket else w
    }


}

object PLA {
    def vectorToString(v: DenseVector[Double]): String = {
        v.foldLeft("")((prefix, x) => prefix + x.toString + " ").toString
    }

    implicit def matrixToList(m :DenseMatrix[Double]): List[DenseVector[Double]] = {
        List.tabulate[DenseVector[Double]](m.rows)(n => m(n, ::).t)
    }

    implicit def doubleVectorToList(v: DenseVector[Double]): List[Double] = {
        v.toArray.toList
    }

    implicit def booleanVectorToList(v: DenseVector[Boolean]): List[Boolean] = {
        v.toArray.toList
    }

    implicit def nestedListToMatrix(list: List[List[Double]]): DenseMatrix[Double] = {
        DenseMatrix(list.map(_.toArray):_*)
        /* List:_* means applly elements in the list to a method as arbitrary number of arguments (varargs) */
    }
}

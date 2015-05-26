import breeze.linalg.{BitVector, Transpose, DenseMatrix, DenseVector}
import breeze.numerics.signum

/**
 * Created by roackb2 on 15/5/26.
 */
case class PLA(X: DenseMatrix[Double], Y: DenseVector[Boolean]) {

    override def toString(): String = {
        Y.toArray.toList.zipWithIndex.foldLeft("")((former, pair) => pair match {
            case (y: Boolean, index: Int) => X(index, ::) match {
                    case Transpose(v) => v match {
                        case e: DenseVector[Double] => former + e.toArray.toList.foldLeft("")((prefix, x) => prefix + x + " ") + " " + y + "\n"
                    }
                }
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
            val index:Int = answer.toArray.toList.zipWithIndex.find{
                case (value, i) => !value
            }.get match {
                case (value, i) => i
            }
            w += (X(index, ::).t  :*= (if(answer(index)) 1.0 else -1.0))
            answer = predict(w) :== Y
        }
        w
    }
}

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.numerics.signum

import scala.util.Random

/**
 * Created by roackb2 on 15/5/24.
 */
object Playground extends App {

    val X: DenseMatrix[Double] = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val Y: DenseVector[Boolean] = DenseVector(true, false)
    val pla1 = PLA(X, Y)
    val pla2 = pla1.add(DenseVector(7.0, 8.0, 9.0), true)
    val pla3 = pla2.addAll(DenseMatrix((10.0, 11.0, 12.0), (13.0, 14.0, 15.0)), DenseVector(false, true))
//    println(pla1)
//    println(pla2)
//    println(pla3)

    val m1 = DenseMatrix(
        (1.0, 2.0, 4.0),
        (3.0 ,5.0, 9.0),
        (5.0, 8.0, 7.0),
        (5.2, 3.7, 2.9))
    val m2 = DenseMatrix((1, 2), (3, 4), (5, 5))

    val v1 = DenseVector(1, -2 ,3, 9, 10)
    val v2 = DenseVector(4, 5, 6, 7, 8)


//    for(i <- 1 to 5) {
//        println(PLA.shuffleRow(m1))
//        println
//    }

    println("iteration count: " + pla3.train)
    println(pla3.w)

    println(pla3.predict)

}

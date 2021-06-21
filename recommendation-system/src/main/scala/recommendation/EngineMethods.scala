package recommendation

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, DiagonalMatrix, EigenDecomposition, RealMatrix}
import org.apache.spark.sql.Row

import scala.collection.mutable
import scala.util.Try

object EngineMethods {

  def createDiagonalAndLaplacianMatrix(dflist: List[RealMatrix]) = {
    val diagonal = for (m <- dflist) yield new DiagonalMatrix(m.getData.map(row => row.sum))
    val laplacian = for ((diag, df) <- diagonal zip dflist) yield df.subtract(diag).scalarMultiply(-1)

    (diagonal, laplacian)
  }

  def mergeIntermediateEmbeddings(intermediateEmbeddings: List[RealMatrix]): RealMatrix = {
    val listSize = intermediateEmbeddings.size
    val isQuadraticMatrix: PartialFunction[RealMatrix, Int] = {
      case x if x.getColumnDimension == x.getRowDimension => x.getRowDimension
    }

    val matrixSize = Try.apply(intermediateEmbeddings collectFirst isQuadraticMatrix getOrElse 0).get

    val consensusEmbedding = new Array2DRowRealMatrix(listSize * matrixSize, listSize * matrixSize)
//    val intermediateEmbeddings = createIntermediateEmbeddings(intermediateEmbeddings, diagonal, laplacian)._1
    var i = 0
    var j = 0
    for (m1 <- intermediateEmbeddings) {
      j = 0
      for (m2 <- intermediateEmbeddings) {
        consensusEmbedding.setSubMatrix(m2.multiply(m1.transpose()).getData, i * matrixSize, j * matrixSize)

        j = j + 1
      }
      i = i + 1
    }

//    calculateConsensusEmbedding(consensusEmbedding)._1
    consensusEmbedding
  }

//  def createConsensusEmbedding(dflist: List[RealMatrix]) = {
//    val listSize = dflist.size
//    val isQuadraticMatrix: PartialFunction[RealMatrix, Int] = {
//      case x if x.getColumnDimension == x.getRowDimension => x.getRowDimension
//    }
//
//    val matrixSize = Try.apply(dflist collectFirst isQuadraticMatrix getOrElse 0).get
//
//    if(dflist.nonEmpty && dflist.head.getRowDimension > 1) {
//      val diagonal = for (m <- dflist) yield new DiagonalMatrix(m.getData.map(row => row.sum))
//      val laplacian = for ((diag, df) <- diagonal zip dflist) yield df.subtract(diag).scalarMultiply(-1) //diag.subtract(df)
//
//
//
//      val previousDfList = dflist
//      val previousDiagonal = diagonal
//      val previousLaplacian = laplacian
//
//      //    println("--------------- CONSENSUS EMBEDING -----------------")
//      //    for (r <- consensusEmbedding.getData) {
//      //      r.foreach(x => print(x + " "))
//      //      println()
//      //    }
//      //    if (stateChanged)
//      val consensusEmbeddingResult = mergeIntermediateEmbeddings()
//
//      Right((consensusEmbeddingResult, dflist, diagonal, laplacian))
//    } else {
////      consensusEmbeddingResult
//      Left("Cannot create recommendation due to lack of neighbours.")
//    }
//  }

//  def intermediateEmbeddings(diagonalMatrix: List[DiagonalMatrix], laplacian: List[RealMatrix]): List[RealMatrix]

  def eigenDecomposition(diagonal: List[DiagonalMatrix], laplacian: List[RealMatrix]) = {
    def normalize(diagonal: DiagonalMatrix, laplacian: RealMatrix): RealMatrix = {
      new Array2DRowRealMatrix(
        laplacian.getData zip Seq.range(0, laplacian.getRowDimension) map (row => row._1.map(d => d/diagonal.getEntry(row._2, row._2)))
      )
    }

    val intermediate = diagonal zip laplacian map(x => normalize(x._1, x._2)) map calculateConsensusEmbedding
    //        val intermediate = dflist map calculateConsensusEmbedding
    val intermediateEmbeddings = intermediate map (x => x._1)
    val eigenValues = intermediate map (x => x._2)
    val eigenVectors = intermediateEmbeddings

    val initialization = false
    (intermediateEmbeddings, eigenValues, initialization)
  }

  def updatedEigenDecomposition(prevDiagonal: List[DiagonalMatrix], currDiagonal: List[DiagonalMatrix],
                                prevLaplacian: List[RealMatrix], currLaplacian: List[RealMatrix],
                                eigenValues: List[Array[Double]], eigenVectors: List[RealMatrix]): (List[RealMatrix], List[Array[Double]], Boolean) = {
    val result = updateEigenValuesAndCorrespondingVectors(
      for ((prev, curr) <- prevDiagonal zip currDiagonal) yield curr.subtract(prev),
      for ((prev, curr) <- prevLaplacian zip currLaplacian) yield curr.subtract(prev),
      eigenValues,
      eigenVectors
    )

    val newEigenValues = result map (x => x._1)
    val newEigenVectors = result map (x => x._2)
    val stateChanged = eigenValues.zip(newEigenValues) exists (x => (x._1 zip x._2 map (y => y._1 - y._2) sum) > 0.0001)

    (newEigenVectors, newEigenValues, stateChanged)
  }

  def createIntermediateEmbeddings(dflist: List[RealMatrix], diagonal: List[DiagonalMatrix], laplacian: List[RealMatrix]) = {
    //      if (initialization) {

    eigenDecomposition(diagonal, laplacian)
    //      } else {
    //        val result = updateEigenValuesAndCorrespondingVectors(
    //          for ((prev, curr) <- previousDiagonal zip diagonal) yield curr.subtract(prev),
    //          for ((prev, curr) <- previousLaplacian zip laplacian) yield curr.subtract(prev),
    //          eigenValues,
    //          eigenVectors
    //        )
    //
    //        stateChanged = eigenValues.zip(result map (x => x._1)) exists (x => (x._1 zip x._2 map (y => y._1 - y._2) sum) > 0.0001)
    //
    //        eigenValues = result map (x => x._1)
    //        eigenVectors = result map (x => x._2)
    //
    //        eigenVectors
    //      }
  }

  def updateEigenValuesAndCorrespondingVectors(diagonalSubtract: List[RealMatrix], laplacianSubtract: List[RealMatrix], eigenValues: List[Array[Double]], eigenVectors: List[RealMatrix]) = {
    def zip4[T1, T2, T3, T4, T](t1: List[T1], t2: List[T2], t3: List[T3], t4: List[T4]) = List(t1, t2, t3, t4).min(Ordering.by[List[Any], Double](_.size)).indices.map(i => (t1(i), t2(i), t3(i), t4(i)))

    val correspondingValuesVectors = zip4(diagonalSubtract, laplacianSubtract, eigenValues, eigenVectors)

    def update(diagonalSubtract: RealMatrix, laplacianSubtract: RealMatrix, values: Array[Double], vectors: RealMatrix): (Array[Double], RealMatrix) = {
      val vectorsMatrixNonNormalized = for(v <- Seq.range(0, vectors.getColumnDimension)) yield new Array2DRowRealMatrix(vectors.getColumn(v))
      val vectorsMatrix = vectorsMatrixNonNormalized.map(vec => vec.scalarMultiply(1/vec.getNorm))
      val multiplyWithV1V2 = (a1: RealMatrix, LD: RealMatrix, a2: RealMatrix) => a1.transpose().multiply(LD).multiply(a2)
      val multiplyWithVVT = (a: RealMatrix, LD: RealMatrix) => multiplyWithV1V2(a, LD, a)
      val eps = 0.000001

//      println()
//      println("Values before : " + values.mkString("Array(", ", ", ")"))
//      println("Vectors before : " + vectors)
//      println("Vectors max: " + vectors.getData.max(Ordering.by[Array[Double], Double](_.max)).mkString("Array(", ", ", ")"))
//      println("Vectors min: " + vectors.getData.min(Ordering.by[Array[Double], Double](_.min)).mkString("Array(", ", ", ")"))
//
//      println()

//      val updVals = values.indices.toList.map(i => multiplyWithVVT(vectorsMatrix(i), laplacianSubtract)
//        .subtract(multiplyWithVVT(vectorsMatrix(i), diagonalSubtract).scalarMultiply(values(i))))

      //omijamy pierwszą wartość własną, zawsze równą 0
      val updatedValues = values.indices.toList.map(i => {
//        println("(" + i + ") a L a: " +  multiplyWithVVT(vectorsMatrix(i), laplacianSubtract))
//        println("(" + i + ") a D a: " + multiplyWithVVT(vectorsMatrix(i), diagonalSubtract))
//        println("(" + i + ") lambda a D a: " + multiplyWithVVT(vectorsMatrix(i), diagonalSubtract).scalarMultiply(values(i)))
//        println("(" + i + ") a L a - lambda a D a: " + multiplyWithVVT(vectorsMatrix(i), laplacianSubtract)
//          .subtract(multiplyWithVVT(vectorsMatrix(i), diagonalSubtract).scalarMultiply(values(i))))
        multiplyWithVVT(vectorsMatrix(i), laplacianSubtract)
        .subtract(multiplyWithVVT(vectorsMatrix(i), diagonalSubtract).scalarMultiply(values(i))).scalarAdd(values(i))}) map (_.getEntry(0,0))

      val uniqueEigenValuesIndexes = (updatedValues zip updatedValues.indices) filter(x => math.abs(x._1 - 1.0) > eps) map(_._2)
//      println("Unique eigen values indexes: " + uniqueEigenValuesIndexes.mkString("Array(", ", ", ")"))

//      for (v <- vectorsMatrix) {
//        println(" Norm: " + v.getNorm)
//      }
      val updatedVectors = vectorsMatrix.indices.toList.map(i => vectorsMatrix(i).add(
//      val updatedVectors = uniqueEigenValuesIndexes.map(i => vectorsMatrix(i).add(
        vectorsMatrix(i)
          .scalarMultiply(-0.5)
          .scalarMultiply(multiplyWithVVT(vectorsMatrix(i), diagonalSubtract).getEntry(0, 0))
          .add(
            vectorsMatrix.indices.toList.filter(j => j != i).map(j =>
//            uniqueEigenValuesIndexes.filter(j => j != i).map(j =>
              {
//                println("values(" + j + ")" + values(j))
//                println("values(j)" + values(j))

//                println("(updatedValues(" + i + ") - updatedValues(" + j + ")" + (updatedValues(i) - updatedValues(j)))
//
//                println("a_j " + vectorsMatrix(i))
//                println("a_i " + vectorsMatrix(j))
//                println("L_A " + laplacianSubtract)
//                println("D_A " + diagonalSubtract)
//
//                println("a_j L_A a_i " + multiplyWithV1V2(vectorsMatrix(i), laplacianSubtract, vectorsMatrix(j)))
//                println("a_j D_A a_i " + multiplyWithV1V2(vectorsMatrix(i), diagonalSubtract, vectorsMatrix(j)))

                vectorsMatrix(j).multiply(multiplyWithV1V2(vectorsMatrix(j), laplacianSubtract, vectorsMatrix(i)) //było multiply
                .subtract(multiplyWithV1V2(vectorsMatrix(i), diagonalSubtract, vectorsMatrix(j))
//                  .scalarMultiply(values(i))).scalarMultiply(1 / (values(i) - values(j)))
                  .scalarMultiply(values(i))).scalarMultiply(1 / (updatedValues(i) - updatedValues(j)))
              )}).reduce((m1, m2) => m1.add(m2))
          )
        )
      )

//      val updVecs = vectorsMatrix.indices.toList.map(i =>
//        vectorsMatrix(i)
//          .scalarMultiply(-0.5)
//          .scalarMultiply(multiplyWithVVT(vectorsMatrix(i), diagonalSubtract).getEntry(0, 0))
//          .add(
//            vectorsMatrix.indices.toList.filter(j => j != i).map(j =>
//            {
//              //                println("values(i)" + values(i))
//              //                println("values(j)" + values(j))
//              //
//              //                println("a_j " + vectorsMatrix(i))
//              //                println("a_i " + vectorsMatrix(j))
//              //                println("L_A " + laplacianSubtract)
//              //                println("D_A " + diagonalSubtract)
//              //
//              //                println("a_j L_A a_i " + multiplyWithV1V2(vectorsMatrix(i), laplacianSubtract, vectorsMatrix(j)))
//              //                println("a_j D_A a_i " + multiplyWithV1V2(vectorsMatrix(i), diagonalSubtract, vectorsMatrix(j)))
//
//              vectorsMatrix(j).multiply(multiplyWithV1V2(vectorsMatrix(j), laplacianSubtract, vectorsMatrix(i)) //było multiply
//                .subtract(multiplyWithV1V2(vectorsMatrix(i), diagonalSubtract, vectorsMatrix(j))
//                  .scalarMultiply(values(i))).scalarMultiply(1 / (values(i) - values(j)))
//              )}).reduce((m1, m2) => m1.add(m2))
//          )
//      )

//      println()
//      println("Values after : " + updatedValues.mkString("Array(", ", ", ")"))
//      print("Vectors after : ")
//      for(v <- updatedVectors) {
//        println(v)
//      }
//      println()
//
//      println()
//      println("Values difference : " + updVals.mkString("Array(", ", ", ")"))
//      println("Vectors difference : " + updVecs)
//      println()

      def joinColumns = (list: List[RealMatrix]) => new Array2DRowRealMatrix(list.map(col => col.getColumn(0)).toArray)

      (updatedValues.toArray, joinColumns(updatedVectors))
//      (updatedValues.map(x => x.getEntry(0,0)).toArray, joinColumns(updatedVectors))
    }

    val updatedEigenValuesVectors = correspondingValuesVectors.map(x => update(x._1, x._2, x._3, x._4))
    updatedEigenValuesVectors toList
  }

  def calculateConsensusEmbedding(consensusEmbedding: RealMatrix) = {

    val result = new EigenDecomposition(consensusEmbedding)
    val eigenValues = result.getRealEigenvalues //filter (x => x > 0) //czy można wykluczyć ujemne?
    val eigenVectors = Seq.range(0, eigenValues.length) map (x => result.getEigenvector(x))

    (collection2DToRealMatrix(eigenVectors map (row => row.toArray.toIterable) toIterable), eigenValues)
  }

  def collection2DToRealMatrix(nested: Iterable[Iterable[Double]]): Array2DRowRealMatrix = {
    val doubleArray = nested map (iter => iter.toArray) toArray

    new Array2DRowRealMatrix(doubleArray)
  }


  def getTopNEmbeddings(embeddings: RealMatrix, dflist: List[RealMatrix]) = {

    if(dflist.nonEmpty) {
      val size = dflist.head.getColumnDimension
      val numOfAttributes = embeddings.getRowDimension
      val topNAttributes = embeddings.getRowDimension / 5 //przyciąć wszytskie do takiej długości, i porównać je
      val p0 = embeddings.getSubMatrix(0, numOfAttributes - 1, 0, size - 1).transpose() //column to : ile wartości
      val p1 = embeddings.getSubMatrix(0, numOfAttributes - 1, size, 2 * size - 1).transpose()
      //    val p2 = embeddings.getSubMatrix(0, numOfAttributes - 1, 2 * size, 3 * size - 1).transpose()
      val sech0_5 = (x: Double) => 2 / (Math.exp(2 * x) + Math.exp(-2 * x))

      val euclides = (x: Iterable[Double], y: Iterable[Double]) => math.sqrt(((for ((a, b) <- x zip y) yield (a - b) * (a - b)) sum))

      def normalize(x: Array[Double]): Array[Double] = {
        val max = x.max
        x map (n => n / max)
      }

      //    val finalEmbeddings = List(dflist(1).multiply(p2).subtract(dflist(2).multiply(p1)),
      //      dflist(2).multiply(p0).subtract(dflist(0).multiply(p2)),
      //      dflist(0).multiply(p1).subtract(dflist(1).multiply(p0)),
      //    )

      val finalEmbeddings = List(dflist(0).multiply(p0).add(dflist(1).multiply(p1)))

      //    val allSimilarities = finalEmbeddings map (_.getRow(0))
//          val resultsPerEmbeddingx = allSimilarities map normalize

      val allSimilarities = finalEmbeddings map (_.getData.map(row => normalize(row.take(topNAttributes).tail)))
      val resultsPerEmbedding = allSimilarities map (m => m.map(row => m.map(row2 => euclides(row, row2))).head)
      //    val joinedResult = resultsPerEmbedding.head.indices.toArray map (x => resultsPerEmbedding map (arr => sech0_5(arr(x) * arr(x))) sum) map math.sqrt
      val joinedResult = resultsPerEmbedding.head.indices.toArray map (x => resultsPerEmbedding map (arr => arr(x) * arr(x)) sum) map math.sqrt map sech0_5

      joinedResult
    } else {
      Array[Double]()
    }
  }

}

import org.scalactic.{Bad, ErrorMessage, Good, Or}

import scala.annotation.tailrec
import scala.util.Try

case class MinAndMaxValues(max: Int, min: Int)

case class MultipleValues(higherVal: Int, lowerVal: Int)

object Day_2 {
    // Exercise 1
    def getCheckSumForRow(row: List[Int]): Int Or ErrorMessage = {
        @tailrec
        def _getCheckSumForRow(row: List[Int], maxAndMin: Option[MinAndMaxValues]): Int Or ErrorMessage = {
            row match {
                case Nil => {
                    maxAndMin match {
                        case Some(minMaxValues) => Good(minMaxValues.max - minMaxValues.min)
                        case None => Bad("No Max or Min Found")
                    }
                }
                case h :: t => {
                    maxAndMin match {
                        case None => {
                            _getCheckSumForRow(
                                row = t,
                                maxAndMin = Option(MinAndMaxValues(max = h, min = h))
                            )
                        }
                        case Some(minAndMaxValues) => {
                            _getCheckSumForRow(
                                row = t,
                                maxAndMin = Option(
                                    MinAndMaxValues(
                                        max = if (h > minAndMaxValues.max) h else minAndMaxValues.max,
                                        min = if (h < minAndMaxValues.min) h else minAndMaxValues.min
                                    )
                                )
                            )
                        }
                    }
                }
            }
        }

        _getCheckSumForRow(row, None)
    }

    def getCheckSumOfMatrix(matrix: List[List[Int]]): Int Or ErrorMessage = {
        val checksumForRows: List[Int Or ErrorMessage] = matrix.map(getCheckSumForRow)

        if (checksumForRows.contains(Bad)) {
            checksumForRows.find(_.isBad).get
        } else {
            Good(checksumForRows.map(_.get).sum)
        }
    }

    def createListOfInts(stringOfInts: String): List[Int] Or ErrorMessage = {
        try {
            val parsedInts: Array[Option[Int]] = stringOfInts.trim.split("\\s+").map(
                (item) => {
                    Try(item.toInt).toOption
                }
            )

            if (parsedInts.contains(None)) {
                Bad("line is unparseable")
            } else {
                Good(parsedInts.map(_.get).toList)
            }
        } catch {
            case e: Exception => Bad("line is unparseable")
        }
    }

    def splitTextIntoRows(stringOfRows: String): List[String] = {
        stringOfRows.split("\n").toList
    }

    def getSolutionFromLinesOfText(stringOfRows: String): Or[Int, ErrorMessage] = {
        val lines: List[Or[List[Int], ErrorMessage]] = splitTextIntoRows(stringOfRows).map(createListOfInts).toList

        if (lines.contains(Bad)) {
            Bad("Unparseable data")
        } else {
            val matrixToParse: List[List[Int]] = lines.map(_.get)
            getCheckSumOfMatrix(matrixToParse)
        }
    }

    // Exercise 2
    @tailrec
    def getMultipleOfInt(num: Int, elementsToMatchAgainst: List[Int]): Option[MultipleValues] = {
        elementsToMatchAgainst match {
            case Nil => None
            case h :: t => {
                if (num % h == 0) {
                    Option(MultipleValues(
                        higherVal = num,
                        lowerVal = h
                    ))
                } else if (h % num == 0) {
                    Option(MultipleValues(
                        higherVal = h,
                        lowerVal = num
                    ))
                } else {
                    getMultipleOfInt(
                        num = num,
                        elementsToMatchAgainst = t
                    )
                }
            }
        }
    }

    @tailrec
    def getMultiplesInLine(line: List[Int]): Option[MultipleValues] = {
        line match {
            case Nil => None
            case h :: t => {
                val result: Option[MultipleValues] = getMultipleOfInt(h, t)
                if (result.isEmpty) {
                    getMultiplesInLine(t)
                } else {
                    result
                }
            }
        }
    }

    def getCheckSumForMultiplesLine(line:List[Int]): Int = {
        val multipleValues = getMultiplesInLine(line)

        multipleValues match {
            case None => 0
            case Some(x) => x.higherVal / x.lowerVal
        }
    }

    def getCheckSumForMultiplesMatrix(matrix: List[List[Int]]): Int = {
        matrix.foldLeft(0)((acc: Int, line: List[Int]) => acc + getCheckSumForMultiplesLine(line))
    }

    def getCheckSumForMultiplesStringOfRows(stringOfRows:String): Or[Int, ErrorMessage] = {
        val lines: List[Or[List[Int], ErrorMessage]] = splitTextIntoRows(stringOfRows).map(createListOfInts).toList

        if (lines.contains(Bad)) {
            Bad("Unparseable data")
        } else {
            val matrixToParse: List[List[Int]] = lines.map(_.get)
            Good(getCheckSumForMultiplesMatrix(matrixToParse))
        }
    }
}

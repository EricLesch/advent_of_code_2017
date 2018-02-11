//    --- Day 2: Corruption Checksum ---
//    As you walk through the door, a glowing humanoid shape yells in your direction. "You there! Your state appears to be idle. Come help us repair the corruption in this spreadsheet - if we take another millisecond, we'll have to display an hourglass cursor!"
//
//    The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.
//
//    For example, given the following spreadsheet:
//
//    5 1 9 5
//    7 5 3
//    2 4 6 8
//    The first row's largest and smallest values are 9 and 1, and their difference is 8.
//    The second row's largest and smallest values are 7 and 3, and their difference is 4.
//    The third row's difference is 6.
//    In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.
//
//    What is the checksum for the spreadsheet in your puzzle input?
//
//    Your puzzle answer was 36174.
//
//    --- Part Two ---
//        "Great work; looks like we're on the right track after all. Here's a star for your effort." However, the program seems a little worried. Can programs be worried?
//
//    "Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."
//
//    It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.
//
//    For example, given the following spreadsheet:
//
//    5 9 2 8
//    9 4 7 3
//    3 8 6 5
//    In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
//    In the second row, the two numbers are 9 and 3; the result is 3.
//    In the third row, the result is 2.
//    In this example, the sum of the results would be 4 + 3 + 2 = 9.
//
//    What is the sum of each row's result in your puzzle input?
//
//    Your puzzle answer was 244.
//
//    Both parts of this puzzle are complete! They provide two gold stars: **
//
//    At this point, you should return to your advent calendar and try another puzzle.
//
//    If you still want to see it, you can get your puzzle input.

import org.scalactic.{Bad, ErrorMessage, Good, Or}

import scala.annotation.tailrec
import scala.util.Try

object Day_2 {
    case class MinAndMaxValues(max: Int, min: Int)

    case class MultipleValues(higherVal: Int, lowerVal: Int)

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

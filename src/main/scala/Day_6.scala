import scala.annotation.tailrec

object Day_6 {
    def findBankRegisterWithMostBlocks(bankRegister: Vector[Int]): Int = {
        bankRegister.indexOf(bankRegister.max)
    }

    @tailrec
    def redistributeBlocks(bankRegister: Vector[Int], indexCount: Int, remainingCount: Int) : Vector[Int] = {
        remainingCount match {
            case 0 => bankRegister
            case _ =>
                {
                    val currentIndex = indexCount % bankRegister.size
                    redistributeBlocks(
                        bankRegister.updated(currentIndex, bankRegister(currentIndex) + 1),
                        indexCount + 1,
                        remainingCount - 1
                    )
                }
        }
    }

    /**
      *
      * @param mapOfResults
      * @param currentResultsKey
      * @param numberOfTimesProcessed
      * @return whether the bank is repeated and the interval between the repeated bank and the last repeated bank
      */
    def detectRepeatedBanks(mapOfResults: Map[String, Int], currentResultsKey: String, numberOfTimesProcessed: Int): (Boolean, Int) = {
        if (mapOfResults.contains(currentResultsKey)) {
            (true, numberOfTimesProcessed - mapOfResults(currentResultsKey))
        } else {
            (false, -1)
        }
    }

    case class ProcessUntilEquilibriumResult(totalNumberOfSteps: Int, distanceBetweenSteps: Int)

    def processUntilEquilibrium(bankRegister: Vector[Int]): ProcessUntilEquilibriumResult = {
        @tailrec
        def _processUntilEquilibrium(bankRegister: Vector[Int], numberOfTimesProcessed: Int, resultHistory: Map[String, Int]): ProcessUntilEquilibriumResult = {
            val indexOfRegisterToRedistribute = findBankRegisterWithMostBlocks(bankRegister)
            val numberOfBanksToRedistribute = bankRegister(indexOfRegisterToRedistribute)

            val newBankRegister = bankRegister.updated(indexOfRegisterToRedistribute, 0)
            val bankRegisterWithRedistributedBlocks = redistributeBlocks(newBankRegister, indexOfRegisterToRedistribute + 1, numberOfBanksToRedistribute)

            val currentResultsKey = bankRegisterWithRedistributedBlocks.toString

            val (hasRepeatedBanksInHistory, distanceSinceRepeated) = detectRepeatedBanks(resultHistory, currentResultsKey, numberOfTimesProcessed)

            val newResultHistory = resultHistory + (currentResultsKey -> numberOfTimesProcessed)

            hasRepeatedBanksInHistory match {
                case true => ProcessUntilEquilibriumResult(
                   totalNumberOfSteps = numberOfTimesProcessed,
                   distanceBetweenSteps = distanceSinceRepeated
                )
                case false => _processUntilEquilibrium(bankRegisterWithRedistributedBlocks, numberOfTimesProcessed + 1, newResultHistory)
            }
        }

        _processUntilEquilibrium(bankRegister, 1, Map())
    }
}

import org.scalatest.FunSpec
import Day_6._

class Day_6_Tests extends FunSpec {
    describe("findBankRegisterWithMostBlocks(bankRegister: Vector[Int]): Int") {
        it("should return the index of the register with the highest values") {
            assert(findBankRegisterWithMostBlocks(Vector(0, 2, 7, 0)) == 2)

            assert(findBankRegisterWithMostBlocks(Vector(2, 4, 1, 2)) == 1)

            assert(findBankRegisterWithMostBlocks(Vector(3, 1, 2, 3)) == 0)
        }
    }

    describe("redistributeBlocks(bankRegister: Vector[Int], indexCount: Int, remainingCount: Int) : Vector[Int]") {
        it("should redistribute blocks to sequential registers") {
            assert(redistributeBlocks(Vector(0, 2, 0, 0), 3, 7) == Vector(2,4,1,2))
        }
    }

    describe("processUntilEquilibrium(bankRegister: Vector[Int]): ProcessUntilEquilibriumResult") {
        it("should return the correct numbers for crunches") {
            val exampleResult = processUntilEquilibrium(Vector(0, 2, 7, 0))

            assert(exampleResult.totalNumberOfSteps == 5)
            assert(exampleResult.distanceBetweenSteps == 4)

            val testResult = processUntilEquilibrium(Vector(4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3))

            assert(testResult.totalNumberOfSteps == 6681)
            assert(testResult.distanceBetweenSteps == 2392)
        }
    }
}

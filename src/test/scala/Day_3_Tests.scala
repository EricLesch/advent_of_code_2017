import org.scalatest.FunSpec
import Day_3._

class Day_3_Tests extends FunSpec {
    // Exercise 1
    describe("_getLevel(count: Int): Int") {
        it("should return the correct data for the passed values") {
            assert(_getLevel(2) == 1)
            assert(_getLevel(8) == 1)
            assert(_getLevel(9) == 1)

            assert(_getLevel(10) == 2)
            assert(_getLevel(25) == 2)

            assert(_getLevel(26) == 3)
            assert(_getLevel(48) == 3)
            assert(_getLevel(49) == 3)

            assert(_getLevel(50) == 4)
        }
    }

    describe("getSteps(count: Int): Int") {
        it("should return the correct data for the passed steps") {
            assert(getSteps(2) == 1)
            assert(getSteps(4) == 1)
            assert(getSteps(5) == 2)
            assert(getSteps(9) == 2)
            assert(getSteps(10) == 3)
            assert(getSteps(17) == 4)

            assert(getSteps(25) == 4)
            assert(getSteps(36) == 5)

            assert(getSteps(361527) == 326)
        }
    }


    val expectedList = List(Direction.RIGHT, Direction.UP, Direction.LEFT, Direction.LEFT, Direction.DOWN, Direction.DOWN, Direction.RIGHT, Direction.RIGHT, Direction.RIGHT, Direction.UP, Direction.UP, Direction.UP, Direction.LEFT, Direction.LEFT, Direction.LEFT, Direction.LEFT, Direction.DOWN, Direction.DOWN, Direction.DOWN, Direction.DOWN)

    describe("_generateDirectionsListImperative(numberOfSteps: Int): List[Direction.Direction]") {
        it("should generate a list of step directions for the the given number of steps") {
            val testList = _generateDirectionsListImperative(20)

            assert(testList == expectedList)
        }
    }

    describe("_generateDirectionsListFunctional(numberOfSteps: Int): List[Direction.Direction]") {
        it("should generate a list of step directions for the the given number of steps") {
            val testList = _generateDirectionsListFunctional(20)

            assert(testList == expectedList)
        }
    }

    describe("pointsAreWithin1Step(point1: Point, point2: Point): Boolean") {
        it("should return true if the points are within 1 step of each other") {
            val testPoint1 = Point(0,0)
            val testPoint2 = Point(1,1)
            val testPoint3 = Point(0,1)
            val testPoint4 = Point(-1,0)
            val testPoint5 = Point(1, 0)

            assert(_pointsAreWithin1Step(testPoint1, testPoint2))
            assert(_pointsAreWithin1Step(testPoint2, testPoint3))
            assert(_pointsAreWithin1Step(testPoint1, testPoint3))

            assert(!_pointsAreWithin1Step(testPoint4, testPoint5))
            assert(!_pointsAreWithin1Step(testPoint2, testPoint4))
        }
    }

    describe("_generateNodeList(numberOfSteps: Int): List[Node]") {
        it("should return the correct points for each node in the generated nodeList ") {
            val nodeList = _generateNodeList(20)

            val pointList: Seq[Day_3.Point] = nodeList.map(_.point)

            val expectedPointList = List(Point(0, 0), Point(1, 0), Point(1, 1), Point(0, 1), Point(-1, 1), Point(-1, 0), Point(-1, -1), Point(0, -1), Point(1, -1), Point(2, -1), Point(2, 0), Point(2, 1), Point(2, 2), Point(1, 2), Point(0, 2), Point(-1, 2), Point(-2, 2), Point(-2, 1), Point(-2, 0), Point(-2, -1), Point(-2, -2))

            assert(pointList == expectedPointList)

            val nodeList2 = _generateNodeList(100)
        }
    }

    describe("getNodeWithHigherValue(value:Int): : Option[Node]") {
        it("should return the first node that has a higher value than the passed value") {
            assert(getNodeWithHigherValue(2).get.value == 4)

            assert(getNodeWithHigherValue(361527).get.value == 363010)
        }
    }
}

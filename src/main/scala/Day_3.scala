//    --- Day 3: Spiral Memory ---
//    You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
//
//    Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
//
//    17  16  15  14  13
//    18   5   4   3  12
//    19   6   1   2  11
//    20   7   8   9  10
//    21  22  23---> ...
//    While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
//
//    For example:
//
//        Data from square 1 is carried 0 steps, since it's at the access port.
//        Data from square 12 is carried 3 steps, such as: down, left, left.
//        Data from square 23 is carried only 2 steps: up twice.
//    Data from square 1024 must be carried 31 steps.
//    How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
//
//    Your puzzle answer was 326.
//
//    --- Part Two ---
//        As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
//
//    So, the first few squares' values are chosen as follows:
//
//        Square 1 starts with the value 1.
//    Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
//    Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
//    Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
//    Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
//    Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
//
//    147  142  133  122   59
//    304    5    4    2   57
//    330   10    1    1   54
//    351   11   23   25   26
//    362  747  806--->   ...
//    What is the first value written that is larger than your puzzle input?
//
//    Your puzzle answer was 363010.
//
//    Both parts of this puzzle are complete! They provide two gold stars: **
//
//    At this point, you should return to your advent calendar and try another puzzle.
//
//    Your puzzle input was 361527.

import Day_3.Direction.Direction

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day_3 {
    // Exercise 1
    // This solution ignores the whole node thing and just works off the pattern that I could see in the generated values
    def getFluctuatingInterval(level: Int, nth: Int): Int = {
        if (nth < level) {
            nth
        } else {
            level - (nth % level)
        }
    }

    def _getLevel(count: Int): Int = {
        scala.math.ceil((scala.math.sqrt(count.toDouble) - 1.0) / 2.0).toInt
    }

    def getSteps(count: Int): Int = {
        val level = _getLevel(count)
        val offset = level - 1

        getFluctuatingInterval(level, (count + offset) % (level * 2)) + level
    }

    // Exercise 2
    case class Point(x: Int, y: Int)

    case class Node(point: Point, value: Int = 0)

    object Pendulum extends Enumeration {
        type Pendulum = Value
        val TICK, TOCK = Value
    }

    object Direction extends Enumeration {
        type Direction = Value
        val UP, DOWN, LEFT, RIGHT = Value
    }

    val DirectionOrder = Array(Direction.RIGHT, Direction.UP, Direction.LEFT, Direction.DOWN)


    // Exercise 2
    def _generateDirectionsListFunctional(numberOfSteps: Int): List[Direction] = {
        @tailrec
        def _generateDirections(directionPointer: Int, numberOfStepsInDirection: Int, numberOfStepsInDirectionCounter: Int, pendulumSwing: Pendulum.Pendulum, totalNumberOfStepsRemainingCounter: Int, acc: List[Direction]): List[Direction] = {
            totalNumberOfStepsRemainingCounter match {
                case 0 => acc
                case _ => {
                    val currentDirectionPointer = directionPointer match {
                        case 4 => 0
                        case _ => directionPointer
                    }

                    val pendulumSwitch = numberOfStepsInDirectionCounter == numberOfStepsInDirection

                    val nextPendulumSwing = if (pendulumSwitch) pendulumSwing match {
                        case Pendulum.TICK => Pendulum.TOCK
                        case Pendulum.TOCK => Pendulum.TICK
                    } else {
                        pendulumSwing
                    }

                    val nextNumberOfStepsInDirectionCounter = if (pendulumSwitch) 1 else numberOfStepsInDirectionCounter + 1

                    val nextDirectionPointer = if (pendulumSwitch) currentDirectionPointer + 1 else currentDirectionPointer

                    val nextNumberOfStepsInDirection = if (pendulumSwitch && pendulumSwing == Pendulum.TOCK) numberOfStepsInDirection + 1 else numberOfStepsInDirection

                    _generateDirections(
                        directionPointer = nextDirectionPointer,
                        numberOfStepsInDirection = nextNumberOfStepsInDirection,
                        numberOfStepsInDirectionCounter = nextNumberOfStepsInDirectionCounter,
                        pendulumSwing = nextPendulumSwing,
                        totalNumberOfStepsRemainingCounter = totalNumberOfStepsRemainingCounter - 1,
                        acc = DirectionOrder(currentDirectionPointer) :: acc
                    )
                }
            }
        }

        _generateDirections(
            directionPointer = 0,
            numberOfStepsInDirection = 1,
            numberOfStepsInDirectionCounter = 1,
            pendulumSwing = Pendulum.TICK,
            totalNumberOfStepsRemainingCounter = numberOfSteps,
            acc = List()
        ).reverse
    }

    def _generateNodeList(numberOfSteps: Int): List[Node] = {
        val stepDirections = _generateDirectionsListFunctional(numberOfSteps)

        val nodeList = List(Node(Point(x = 0, y = 0), value = 1))

        addNodes(stepDirections, nodeList).reverse
    }

    def addNodes(directionsList: List[Direction.Direction], acc: List[Node]): List[Node] = {
        directionsList.foldLeft(acc)(
            (acc: List[Node], direction: Direction.Direction) => {
                val accHeadPoint = acc.head.point
                val newNodePoint = direction match {
                    case Direction.RIGHT => Point(accHeadPoint.x + 1, accHeadPoint.y)
                    case Direction.UP => Point(accHeadPoint.x, accHeadPoint.y + 1)
                    case Direction.LEFT => Point(accHeadPoint.x - 1, accHeadPoint.y)
                    case Direction.DOWN => Point(accHeadPoint.x, accHeadPoint.y - 1)
                }

                val adjacentNodes: List[Node] = acc.filter((node: Node) => _pointsAreWithin1Step(node.point, newNodePoint))

                Node(
                    point = newNodePoint,
                    value = adjacentNodes.map(_.value).sum
                ) :: acc
            }
        )
    }

    def _pointsAreWithin1Step(point1: Point, point2: Point): Boolean = {
        math.abs(point1.x - point2.x) <= 1 &&
            math.abs(point1.y - point2.y) <= 1
    }

    def getNodeWithHigherValue(value: Int): Option[Node] = {
        // this is a bit lazy and definitely suboptimal - more optimal would be progressively generating nodes until you get the value you need
        _generateNodeList(200).find((node: Node) => node.value > value)
    }
}

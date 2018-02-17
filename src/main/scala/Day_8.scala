//    --- Day 8: I Heard You Like Registers ---
//        You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like you to compute the result of a series of unusual register instructions.
//
//        Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:
//
//        b inc 5 if a > 1
//    a inc 1 if b < 5
//    c dec -10 if a >= 1
//    c inc -20 if c == 10
//    These instructions would be processed as follows:
//
//        Because a starts at 0, it is not greater than 1, and so b is not modified.
//    a is increased by 1 (to 1) because b is less than 5 (it is 0).
//        c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
//        c is increased by -20 (to -10) because c is equal to 10.
//    After this process, the largest value in any register is 1.
//
//    You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.
//
//        What is the largest value in any register after completing the instructions in your puzzle input?
//
//    Your puzzle answer was 3880.
//
//    --- Part Two ---
//        To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).
//
//        Your puzzle answer was 5035.
//
//    Both parts of this puzzle are complete! They provide two gold stars: **
//
//    At this point, you should return to your advent calendar and try another puzzle.

import scala.annotation.tailrec

object Day_8 {

    object Command extends Enumeration {
        type Command = Value
        val INCREMENT, DECREMENT = Value
    }

    object ConditionOperation extends Enumeration {
        type ConditionOperation = Value
        val EQUAL, GREATER_THAN, GREATER_THAN_OR_EQUAL_TO, LESS_THAN, LESS_THAN_OR_EQUAL_TO, NOT_EQUAL_TO = Value
    }

    def splitIntoLines(data: String): List[String] = {
        data.split("\n").map(_.trim).toList
    }

    //    b inc 5 if a > 1
    case class LineData(
                           registerToModify: String,
                           command: Command.Command,
                           amount: Int,
                           registerCondition: String,
                           conditionOperation: ConditionOperation.ConditionOperation,
                           conditionAmount: Int
                       )

    def splitLineIntoCommands(line: String): LineData = {
        val chunks: Array[String] = line.trim.split(" ").map(_.trim)

        val registerToModify: String = chunks(0)

        val command: Command.Command = chunks(1) match {
            case "dec" => Command.DECREMENT
            case "inc" => Command.INCREMENT
        }

        val amount = chunks(2).toInt

        val registerCondition: String = chunks(4)

        val conditionOperation = chunks(5) match {
            case ">" => ConditionOperation.GREATER_THAN
            case "<" => ConditionOperation.LESS_THAN
            case "==" => ConditionOperation.EQUAL
            case ">=" => ConditionOperation.GREATER_THAN_OR_EQUAL_TO
            case "<=" => ConditionOperation.LESS_THAN_OR_EQUAL_TO
            case "!=" => ConditionOperation.NOT_EQUAL_TO
        }

        val conditionAmount = chunks(6).toInt

        LineData(
            registerToModify = registerToModify,
            command = command,
            amount = amount,
            registerCondition = registerCondition,
            conditionOperation = conditionOperation,
            conditionAmount = conditionAmount
        )
    }

    def getRegisters(commands: List[LineData]) : List[String] = {
        commands.flatMap((command) => Array(command.registerToModify, command.registerCondition)).distinct
    }

    def getAllRegisters(data: String): List[String] = {
        val lines = splitIntoLines(data)

        val commands: List[LineData] = lines.map(splitLineIntoCommands)

        getRegisters(commands)
    }

    def initializeRegisterMap(registerNames: List[String]): Map[String, Int] = {
        registerNames.map((registerName) => (registerName, 0)).toMap
    }

    case class RunCommandResult(registerValues: Map[String, Int], maxValue: Int)

    @tailrec
    def _runCommands(allLineData: List[LineData], registerValues: Map[String, Int], highestValueAtAnyPoint: Int): RunCommandResult = {
        allLineData match {
            case Nil => RunCommandResult(registerValues, highestValueAtAnyPoint)
            case h :: t => {
                val conditionRegisterValue = registerValues(h.registerCondition)

                val bool = h.conditionOperation match {
                    case ConditionOperation.NOT_EQUAL_TO => conditionRegisterValue != h.conditionAmount
                    case ConditionOperation.EQUAL => conditionRegisterValue == h.conditionAmount
                    case ConditionOperation.GREATER_THAN_OR_EQUAL_TO => conditionRegisterValue >= h.conditionAmount
                    case ConditionOperation.GREATER_THAN => conditionRegisterValue > h.conditionAmount
                    case ConditionOperation.LESS_THAN => conditionRegisterValue < h.conditionAmount
                    case ConditionOperation.LESS_THAN_OR_EQUAL_TO => conditionRegisterValue <= h.conditionAmount
                }

                val (newRegisterValues, newHighestValueAtAnyPoint) = if (bool) {
                    val currentRegisterValue: Int = registerValues(h.registerToModify)

                    val newAmount = h.command match {
                        case Command.INCREMENT => currentRegisterValue + h.amount
                        case Command.DECREMENT => currentRegisterValue - h.amount
                    }

                    val newRegister = registerValues + (h.registerToModify -> newAmount)

                    (newRegister, Array(newAmount, highestValueAtAnyPoint).max)
                } else {
                    (registerValues, highestValueAtAnyPoint)
                }

                _runCommands(t, newRegisterValues, newHighestValueAtAnyPoint)
            }
        }
    }

    def runCommands(allLineData: List[LineData], registers: List[String]): RunCommandResult = {
       _runCommands(allLineData, initializeRegisterMap(registers), 0)
    }

    def processData(data:String): RunCommandResult = {
        val lines = splitIntoLines(data)

        val commands: List[LineData] = lines.map(splitLineIntoCommands)

        runCommands(commands, getRegisters(commands))
    }

    // Exercise 1 entry point
    def getLargestValueInAnyRegisterForData(data:String): Int = {
        processData(data).registerValues.maxBy(_._2)._2
    }

    // Exercise 2 entry point
    def getLargestValueAtAnyPoint(data: String): Int = {
        processData(data).maxValue
    }
}

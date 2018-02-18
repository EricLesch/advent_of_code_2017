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

    /**
      * Splits the data and returns a list of lines that can be parsed into commands
      * @param data
      * @return
      */
    def splitIntoLines(data: String): List[String] = {
        data.split("\n").map(_.trim).toList
    }

    /**
      * Contains all the relevant information of a command that is needed to execute the command
      * @param registerToModify the register to increment or decrement if the condition evaluates to true
      * @param command - the operation increment or decrement
      * @param amount - the amount to increment or decrement
      * @param conditionRegister - the register to test - ie apply the conditionOperator
      * @param conditionOperator - the test to apply to the register (e.g. <, ==, >, etc)
      * @param conditionValue - the value that the conditionOperator is applying to the conditionRegister
      */
    case class CommandData(
                              registerToModify: String,
                              command: Command.Command,
                              amount: Int,
                              conditionRegister: String,
                              conditionOperator: ConditionOperation.ConditionOperation,
                              conditionValue: Int
                       )

    /**
      * Parse a line of data and
      * Returns a CommandData object which contains all of the relevant data needed to execute the command
      * @param line
      * @return
      */
    def splitLineIntoCommands(line: String): CommandData = {
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

        CommandData(
            registerToModify = registerToModify,
            command = command,
            amount = amount,
            conditionRegister = registerCondition,
            conditionOperator = conditionOperation,
            conditionValue = conditionAmount
        )
    }

    /**
      * Takes a list of all commands and returns a list of the names of all the referenced registers
      * @param commands
      * @return
      */
    def getRegisters(commands: List[CommandData]) : List[String] = {
        commands.flatMap((command) => Array(command.registerToModify, command.conditionRegister)).distinct
    }

    /**
      * Takes passed the input data and
      * Returns the names of all the registers in the application
      * @param data
      * @return
      */
    def getAllRegisters(data: String): List[String] = {
        val lines = splitIntoLines(data)

        val commands: List[CommandData] = lines.map(splitLineIntoCommands)

        getRegisters(commands)
    }

    /**
      * Takes the names of all the registers and
      * Returns a map with the register names as keys and all values initialized to zero
      * @param registerNames
      * @return
      */
    def initializeRegisterMap(registerNames: List[String]): Map[String, Int] = {
        registerNames.map((registerName) => (registerName, 0)).toMap
    }

    /**
      * Contains the values needed to complete Exercise 1 and 2
      * @param registerValues - the values of each register at the end of computation
      * @param maxValue - the maximum value that any register held at any point during the computation
      */
    case class ExecutionResult(registerValues: Map[String, Int], maxValue: Int)

    /**
      * Runs all of the commands and
      * Returns the values of the registers at the end and the highest value encountered during computation
      * @param allCommands - the remaining commands that need to be processed
      * @param registerValues - the current map of registers to values
      * @param highestValueAtAnyPoint - the current highest value encountered in any register at any point in the computation
      * @return
      */
    @tailrec
    def _runCommands(allCommands: List[CommandData], registerValues: Map[String, Int], highestValueAtAnyPoint: Int): ExecutionResult = {
        allCommands match {
            case Nil => ExecutionResult(registerValues, highestValueAtAnyPoint)
            case h :: t => {
                val conditionRegisterValue = registerValues(h.conditionRegister)

                val bool = h.conditionOperator match {
                    case ConditionOperation.NOT_EQUAL_TO => conditionRegisterValue != h.conditionValue
                    case ConditionOperation.EQUAL => conditionRegisterValue == h.conditionValue
                    case ConditionOperation.GREATER_THAN_OR_EQUAL_TO => conditionRegisterValue >= h.conditionValue
                    case ConditionOperation.GREATER_THAN => conditionRegisterValue > h.conditionValue
                    case ConditionOperation.LESS_THAN => conditionRegisterValue < h.conditionValue
                    case ConditionOperation.LESS_THAN_OR_EQUAL_TO => conditionRegisterValue <= h.conditionValue
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

    /**
      * Takes a list of all the commands and registers and runs all of the computations
      * Returns the result of the computation
      * @param allCommands - all of the commands that need to be processed
      * @param registerNames - all of the names of the registers
      * @return
      */
    def runCommands(allCommands: List[CommandData], registerNames: List[String]): ExecutionResult = {
       _runCommands(allCommands, initializeRegisterMap(registerNames), 0)
    }

    /**
      * Parses all of the data and then processes it
      * Returns the result of the computation
      * @param data - lines of text containing the commands
      * @return
      */
    def processData(data:String): ExecutionResult = {
        val lines = splitIntoLines(data)

        val commands: List[CommandData] = lines.map(splitLineIntoCommands)

        runCommands(commands, getRegisters(commands))
    }

    /**
      * Exercise 1 entry point
      * Returns the max value in any register at the end of computation
      * @param data
      * @return
      */
    def getLargestValueInAnyRegisterForData(data:String): Int = {
        processData(data).registerValues.maxBy(_._2)._2
    }

    /**
      * Exercise 2 entry point
      * Returns the max value in any register at any point of the computation
      * @param data
      * @return -
      */
    def getLargestValueAtAnyPoint(data: String): Int = {
        processData(data).maxValue
    }
}

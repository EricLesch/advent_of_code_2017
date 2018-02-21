import scala.annotation.tailrec

object Day_9 {

    /**
      *
      * @param value - character to be parsed
      * @param flaggedForRemoval - whether or not this character should be removed in this pass
      */
    case class CharacterContainer(value: Char, flaggedForRemoval: Boolean = false)

    /**
      * Transforms the input data into a list of flaggable character containers
      * @param data
      * @return
      */
    def getListOfCharacterContainers(data: String): List[CharacterContainer] = {
        data.map(CharacterContainer(_)).toList
    }

    /**
      * @param groupsAtThisLevel - number of groups in a given level
      * @param resultList - the results with all of the characters that are flagged for removal
      */
    case class ProcessCharactersInLevelResult(groupsAtThisLevel: Int, resultList: List[CharacterContainer]) {
        /**
          * toString for easier debugging
          * @return
          */
        override def toString: String = {
            s"GroupsAtThisLevel=${groupsAtThisLevel}\nresultList=${resultList.map(_.value).mkString}"
        }
    }

    /**
      * Marks a character container for removal
      * @param characterContainer - the container to be marked
      * @return - an identical container that is marked
      */
    def markForRemoval(characterContainer: CharacterContainer): CharacterContainer = {
        CharacterContainer(characterContainer.value, true)
    }

    /**
      * Processes each characters, adding up the number of groups at this level, and flags for removal all characters that
      * aren't relevant for the next level of processing
      * @param listToParse - list of containers that contain all of the characters in the string to be parsed
      * @return - a list of characters that can be processed in the next level and the group score for this level
      */
    def _processCharacters(listToParse: List[CharacterContainer], acc: List[CharacterContainer], bracketCounter: Int, isInsideGarbage: Boolean, groupsAtThisLevel: Int): ProcessCharactersInLevelResult = {
        listToParse match {
            case Nil => ProcessCharactersInLevelResult(groupsAtThisLevel, acc.reverse)
            case h :: t => {
                h.value match {
                    case '{' => {
                        val (newBracketCounter, newHeadOfAcc) = if (isInsideGarbage) {
                            (bracketCounter, markForRemoval(h))
                        } else {
                            val newBracketCounter = bracketCounter + 1
                            val newHeadOfAcc = if (bracketCounter == 0) {
                                markForRemoval(h)
                            } else {
                                h
                            }
                            (newBracketCounter, newHeadOfAcc)
                        }
                        _processCharacters(t, newHeadOfAcc :: acc, newBracketCounter, isInsideGarbage, groupsAtThisLevel)
                    }
                    case '}' => {
                        val (newBracketCounter, newHeadOfAcc, newGroupsAtThisLevel) = if (isInsideGarbage) {
                            (bracketCounter, markForRemoval(h), groupsAtThisLevel)
                        } else {
                            val newCounter = bracketCounter - 1
                            val (newHeadOfAcc, newGroupsAtThisLevel) = if (newCounter == 0 && !isInsideGarbage) {
                                (markForRemoval(h), groupsAtThisLevel + 1)
                            } else {
                                (h, groupsAtThisLevel)
                            }
                            (newCounter, newHeadOfAcc, newGroupsAtThisLevel)
                        }
                        _processCharacters(t, newHeadOfAcc :: acc, newBracketCounter, isInsideGarbage, newGroupsAtThisLevel)
                    }
                    case '<' => {
                        val newGarbageCounter = true

                        _processCharacters(t, markForRemoval(h) :: acc, bracketCounter, newGarbageCounter, groupsAtThisLevel)
                    }
                    case '>' => {
                        val newGarbageCounter = false

                        _processCharacters(t, markForRemoval(h) :: acc, bracketCounter, newGarbageCounter, groupsAtThisLevel)
                    }
                    case '!' => {
                        val first = markForRemoval(h)
                        val second = markForRemoval(t.head)

                        _processCharacters(t.tail, first :: second :: acc.tail, bracketCounter, isInsideGarbage, groupsAtThisLevel)
                    }
                    case _ => {
                        _processCharacters(t, markForRemoval(h) :: acc, bracketCounter, isInsideGarbage, groupsAtThisLevel)
                    }
                }
            }
        }
    }

    /**
      * Processes all the characters, counting the number of groups at this level, and removes all characters that
      * aren't relevant for the next level of processing
      * @param listToParse - list of containers that contain all of the characters in the string to be parsed
      * @return - a list of characters that can be processed in the next level and the group score for this level
      */
    def processCharactersInLevel(listToParse: List[CharacterContainer]): ProcessCharactersInLevelResult = {
        val result = _processCharacters(listToParse, Nil, 0, false, 0)
        val charactersWithOutRemovedCharacters = result.resultList.filter(!_.flaggedForRemoval)

        ProcessCharactersInLevelResult(
            result.groupsAtThisLevel,
            charactersWithOutRemovedCharacters
        )
    }

    /**
      * Takes a list to process and returns the group score
      * @param listToParse - list of containers that contain all of the characters in the string to be parsed
      * @param acc - result accumulator
      * @param level - current level of processing - higher levels add more points
      * @return - the sum of all the scores in all of the groups
      */
    @tailrec
    def getTotalGroupScore(listToParse: List[CharacterContainer], acc: Int, level : Int) : Int = {
        listToParse match {
            case Nil => acc
            case _ => {
                val stringToMatch: String = listToParse.map(_.value).mkString

                if (stringToMatch.contains("{"))
                {
                    val result = processCharactersInLevel(listToParse)
                    getTotalGroupScore(result.resultList, (result.groupsAtThisLevel * level) + acc, level + 1)
                } else {
                    acc
                }
            }
        }
    }

    /**
      * Exercise 1 Entry point
      * @param data - data that gets parsed
      * @return - the score of all of the groups
      */
    def parseStrings(data: String): Int = {
        val characterContainers = getListOfCharacterContainers(data)

        getTotalGroupScore(characterContainers, 0, 1)
    }
}

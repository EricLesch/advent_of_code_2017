//    Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves into a bit of trouble. A recursive algorithm has gotten out of hand, and now they're balanced precariously in a large tower.
//
//        One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the disc below them balanced but with no disc of their own.
//
//        You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.
//
//    For example, if your list is the following:
//
//        pbga (66)
//    xhth (57)
//    ebii (61)
//    havc (66)
//    ktlj (57)
//    fwft (72) -> ktlj, cntj, xhth
//    qoyq (66)
//    padx (45) -> pbga, havc, qoyq
//    tknk (41) -> ugml, padx, fwft
//    jptl (61)
//    ugml (68) -> gyxo, ebii, jptl
//    gyxo (61)
//    cntj (57)
//    ...then you would be able to recreate the structure of the towers that looks like this:
//
//        gyxo
//    /
//    ugml - ebii
//    /      \
//        |         jptl
//        |
//    |         pbga
//        /        /
//            tknk --- padx - havc
//    \        \
//        |         qoyq
//        |
//    |         ktlj
//        \      /
//            fwft - cntj
//    \
//    xhth
//    In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)
//
//    Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?
//
//    Your puzzle answer was gynfwly.
//
//        --- Part Two ---
//        The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's fixed, they're stuck here.
//
//        For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.
//
//        In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same weight, and they do: 61.
//
//    However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match. This means that the following sums must all be the same:
//
//    ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
//    padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
//    fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
//    As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.
//
//    Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?
//
//    Your puzzle answer was 1526.
//
//    Both parts of this puzzle are complete! They provide two gold stars: **


import scala.annotation.tailrec

object Day_7 {

    abstract class Node[+A] {
        def name: String
    }

    case class Branch[A](
                            name: String,
                            value: A,
                            nodeNames: List[String],
                            nodes: List[Node[A]] = Nil) extends Node

    case class Leaf[A](
                          name: String,
                          value: A) extends Node

    /**
      * Parses data and returns a brnach
      * @param line
      * @return
      */
    def parseBranch(line: String): Branch[Int] = {
        val parts = line.split("->")
        val nameAndValue: Array[String] = parts(0).trim.split(" ")
        val nodesNames: Array[String] = parts(1).trim.split(", ")

        Branch[Int](
            name = nameAndValue(0),
            value = nameAndValue(1).dropRight(1).drop(1).toInt,
            nodeNames = nodesNames.toList
        )
    }

    /**
      * Parses data and returns a leaf
      * @param line
      * @return
      */
    def parseLeaf(line: String): Leaf[Int] = {
        val nameAndValue: Array[String] = line.trim.split(" ")

        Leaf[Int](
            name = nameAndValue(0),
            value = nameAndValue(1).dropRight(1).drop(1).toInt
        )
    }

    case class BranchesAndLeaves(branches: List[Branch[Int]], leaves: List[Leaf[Int]])

    /**
      * parses the data and returns two separated lists of branches and leaves
      * @param data
      * @return
      */
    def getBranchesAndLeaves(data: String): BranchesAndLeaves = {
        val lines: List[String] = data.split("\n").toList

        def isBranch(line: String): Boolean = {
            line.contains("->")
        }

        @tailrec
        def _getBranchesAndLeaves(data: List[String], acc: BranchesAndLeaves): BranchesAndLeaves = {
            data match {
                case Nil => acc
                case h :: t => {
                    val newAcc = if (isBranch(h)) {
                        val newBranch = parseBranch(h)

                        acc.copy(
                            branches = newBranch :: acc.branches
                        )
                    } else {
                        val newLeaf = parseLeaf(h)

                        acc.copy(
                            leaves = newLeaf :: acc.leaves
                        )
                    }

                    _getBranchesAndLeaves(
                        data = t,
                        acc = newAcc
                    )
                }
            }
        }

        _getBranchesAndLeaves(
            data = lines,
            acc = BranchesAndLeaves(
                branches = List(),
                leaves = List()
            )
        )
    }

    /**
      * Returns a map of node names and the names of their corresponding parent nodes
      * @param listOfBranches
      * @return
      */
    def getMapOfChildrenToParents(listOfBranches: List[Branch[Int]]): Map[String, String] = {

        @tailrec
        def _getMapOfChildrenToParents(listOfBranches: List[Branch[Int]], acc: Map[String, String]): Map[String, String] = {
            listOfBranches match {
                case Nil => acc
                case h :: t => {
                    _getMapOfChildrenToParents(t, acc ++ h.nodeNames.map(nodeName => (nodeName, h.name)))
                }
            }
        }

        _getMapOfChildrenToParents(listOfBranches, Map())
    }

    /**
      * Returns the name of the root node of the tree
      * @param childToParentMap
      * @return
      */
    def getRootNode(childToParentMap: Map[String, String]): String = {

        @tailrec
        def _getRootNode(childToParentMap: Map[String, String], currentName: String): String = {
            if (!childToParentMap.contains(currentName)) {
                currentName
            } else {
                _getRootNode(childToParentMap, childToParentMap(currentName))
            }
        }

        _getRootNode(childToParentMap, childToParentMap.keys.last)
    }

    //  Exercise 1 entry point
    def getRootNodeNameFromData(data: String): String = {
        val listOfNodes = getBranchesAndLeaves(data)

        val mapOfChildrenToParents = getMapOfChildrenToParents(listOfNodes.branches)

        getRootNode(mapOfChildrenToParents)
    }

    /**
      * Determines if all of the children of this node are connected to this node
      * @param node
      * @tparam A
      * @return
      */
    def hasAllChildrenConnected[A](node: Node[A]): Boolean = {
        node match {
            case _: Leaf[_] => true
            case branch: Branch[_] => branch.nodes.length == branch.nodeNames.size
        }
    }

    /**
      * Determines if the children of the given node have all of *their* children connected already
      * @param node
      * @param mapOfNodes
      * @tparam A
      * @return
      */
    def immediateChildrenHaveAllChildrenConnected[A](node: Node[A], mapOfNodes: Map[String, Node[A]]): Boolean = {
        node match {
            case _: Leaf[_] => true
            case branch: Branch[_] => branch.nodeNames.forall(
                (nodeName: String) => {
                    hasAllChildrenConnected(mapOfNodes(nodeName))
                }
            )
        }
    }

    /**
      * Assembles a tree from a map of Nodes, the first node should be the root node of the returned tree
      * @param mapOfNodes  - a map of nodenames to Nodes
      * @param currentNode - the root node of the tree
      * @tparam A
      * @return
      */
    def assembleNodes[A](mapOfNodes: Map[String, Node[A]], currentNode: Node[A]): Map[String, Node[A]] = {
        val allChildrenAreLinkedUp = immediateChildrenHaveAllChildrenConnected(currentNode, mapOfNodes)
        if (allChildrenAreLinkedUp) {
            val newNode = currentNode match {
                case leaf: Leaf[_] => leaf
                case branch: Branch[_] => branch.copy(nodes = branch.nodeNames.map(mapOfNodes(_)))
            }
            mapOfNodes + (newNode.name -> newNode)
        } else {
            currentNode match {
                case _: Leaf[_] => mapOfNodes
                case branch: Branch[_] => {
                    val unconnectedNodeNames =
                        branch.nodeNames.filter(
                            (nodeName: String) => !hasAllChildrenConnected(mapOfNodes(nodeName))
                        )

                    val updatedMapOfNodes: Map[String, Node[A]] = unconnectedNodeNames.foldLeft(mapOfNodes)(
                        (mapOfNodes, nodeName: String) => assembleNodes(mapOfNodes, mapOfNodes(nodeName))
                    )

                    val newBranch = branch.copy(
                        nodes = branch.nodeNames.map(updatedMapOfNodes(_))
                    )

                    updatedMapOfNodes + (newBranch.name -> newBranch)
                }
            }
        }
    }

    /**
      * For a given node, sum the value of the node and the values of all descendant nodes
      * @param node
      * @return
      */
    def getSumOfNode(node: Node[Int]): Int = {
        node match {
            case branch: Branch[Int] => {
                branch.value + branch.nodes.foldLeft(0)((acc, node: Node[Int]) => acc + getSumOfNode(node))
            }
            case leaf: Leaf[Int] => leaf.value
        }
    }

    /**
      * Construct a tree from the passed lists of branches and leaves
      * @param listOfNodes
      * @tparam A
      * @return the root node of the tree
      */
    def constructTree[A](listOfNodes: BranchesAndLeaves): Node[A] = {
        val rootNodeName: String = getRootNode(getMapOfChildrenToParents(listOfNodes.branches))

        val listOfAllNodes: List[Node[A]] = listOfNodes.branches ::: listOfNodes.leaves

        val mapOfNodes: Map[String, Node[A]] = listOfAllNodes.map((node: Node[A]) => (node.name, node)).toMap

        val assembledMapOfNodes = assembleNodes(mapOfNodes, mapOfNodes(rootNodeName))

        assembledMapOfNodes(rootNodeName)
    }

    case class FindUnbalancedBranchResult(
                                             parent: Node[Int],
                                             unbalancedChild: Node[Int],
                                             otherChildren: List[Node[Int]],
                                             unbalancedChildSum: Int,
                                             otherChildrenSum: Int)

    /**
      * Searches down through the tree until it finds the deepest instance of an unbalanced branch
      * @param node
      * @param currentResult
      * @return information relating to the unbalanced branch and its children
      */
    @tailrec
    def findUnbalancedBranch(node: Node[Int], currentResult: Option[FindUnbalancedBranchResult]): Option[FindUnbalancedBranchResult] = {
        node match {
            case branch: Branch[Int] => {
                val sumMap: Map[Int, List[Int]] = branch.nodes.map(getSumOfNode).groupBy(identity)
                if (sumMap.toSet.size != 1) {  // the sums of the children nodes aren't all the same
                    val leastCommonSum: Int = sumMap.minBy(_._2.size)._1
                    val mostCommonSum: Int = sumMap.maxBy(_._2.size)._1
                    val unbalancedChild: Node[Int] = branch.nodes.filter(getSumOfNode(_) == leastCommonSum).head
                    val otherChildren: List[Day_7.Node[Int]] = branch.nodes.filter(getSumOfNode(_) == mostCommonSum)

                    findUnbalancedBranch(
                        unbalancedChild,
                        Some(
                            FindUnbalancedBranchResult(
                                parent = branch,
                                unbalancedChild = unbalancedChild,
                                otherChildren = otherChildren,
                                unbalancedChildSum = getSumOfNode(unbalancedChild),
                                otherChildrenSum = getSumOfNode(otherChildren.head)
                            )
                        )
                    )
                } else { // the branches are balanced
                    currentResult
                }
            }
            case _: Leaf[Int] => None
        }
    }

    /**
      * Parses the data and returns the root node of the tree
      * @param data
      * @tparam A
      * @return
      */
    def constructTreeFromData[A](data: String): Node[A] = {
        val listOfNodes = getBranchesAndLeaves(data)

        constructTree(listOfNodes)
    }

    /**
      * Gets the value of a node using pattern matching - I had a hard time moving the property *value* up to the Node class so this is
      * the crappy workaround. Clearly there must be a better way to do this but I am a noob at this point and not experienced
      * with covariant abstract classes
      * @param node
      * @return
      */
    def getValueOfNode(node: Node[Int]): Int = {
        node match {
            case branch: Branch[Int] => branch.value
            case leaf: Leaf[Int] => leaf.value
        }
    }

    /**
      * @param nodeName the name of the unbalanced node
      * @param correctValue what the value of the node should be to have a balanced tree
      */
    case class Exercise2Result(nodeName: String, correctValue: Int)

    // Exercise 2 Entry Point
    def getValueThatUnbalancedNodeShouldBe(data: String): Option[Exercise2Result] = {
        val rootNode = constructTreeFromData(data)

        val unbalancedSearchResults: Option[FindUnbalancedBranchResult] = findUnbalancedBranch(rootNode, None)

        unbalancedSearchResults match {
            case None => None
            case Some(result) => Some(
                Exercise2Result(
                    nodeName = result.unbalancedChild.name,
                    correctValue = getValueOfNode(result.unbalancedChild) - (result.unbalancedChildSum - result.otherChildrenSum)
                )
            )
        }
    }
}

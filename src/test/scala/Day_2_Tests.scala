import org.scalatest.FunSpec
import Day_2._
import org.scalactic
import org.scalactic.ErrorMessage

class Day_2_Tests extends FunSpec {
    describe("Exercise 1") {
        val firstRow = List(5, 1, 9, 5)
        val secondRow = List(7, 5, 3)
        val thirdRow = List(2, 4, 6, 8)

        val testData =
            """3751	3769	2769	2039	2794	240	3579	1228	4291	220	324	3960	211	1346	237	1586
                550	589	538	110	167	567	99	203	524	288	500	111	118	185	505	74
                2127	1904	199	221	1201	250	1119	377	1633	1801	2011	1794	394	238	206	680
                435	1703	1385	1461	213	1211	192	1553	1580	197	571	195	326	1491	869	1282
                109	104	3033	120	652	2752	1822	2518	1289	1053	1397	951	3015	3016	125	1782
                2025	1920	1891	99	1057	1909	2237	106	97	920	603	1841	2150	1980	1970	88
                1870	170	167	176	306	1909	1825	1709	168	1400	359	817	1678	1718	1594	1552
                98	81	216	677	572	295	38	574	403	74	91	534	662	588	511	51
                453	1153	666	695	63	69	68	58	524	1088	75	1117	1192	1232	1046	443
                3893	441	1825	3730	3660	115	4503	4105	3495	4092	48	3852	132	156	150	4229
                867	44	571	40	884	922	418	328	901	845	42	860	932	53	432	569
                905	717	162	4536	4219	179	990	374	4409	4821	393	4181	4054	4958	186	193
                2610	2936	218	2552	3281	761	204	3433	3699	2727	3065	3624	193	926	1866	236
                2602	216	495	3733	183	4688	2893	4042	3066	3810	189	4392	3900	4321	2814	159
                166	136	80	185	135	78	177	123	82	150	121	145	115	63	68	24
                214	221	265	766	959	1038	226	1188	1122	117	458	1105	1285	1017	274	281"""

        describe("getCheckSumForRow(row: List[Int]") {
            it("should calculate the checksums of each row") {

                assert(getCheckSumForRow(firstRow).getOrElse(-1) == 8)
                assert(getCheckSumForRow(secondRow).getOrElse(-1) == 4)
                assert(getCheckSumForRow(thirdRow).getOrElse(-1) == 6)
            }
        }

        describe("getCheckSumOfMatrix(matrix:List[List[Int]])") {
            it("should passed when given the correct data") {
                assert(
                    getCheckSumOfMatrix(
                        List(
                            firstRow,
                            secondRow,
                            thirdRow
                        )
                    ).getOrElse(-1) == 18
                )

            }
        }

        describe("createListOfInts(stringOfInts:String): List[Int] Or ErrorMessage") {
            it("should return a list when passed a string") {
                val testRow = "3751	3769	2769	2039	2794	240	3579	1228	4291	220	324	3960	211	1346	237	1586"

                val listOfInts: scalactic.Or[List[Int], ErrorMessage] = createListOfInts(testRow)

                assert(listOfInts.getOrElse(Nil).nonEmpty)
            }

            it("should return an error is passed bad data") {
                val testRow = "something 1234"

                val listOfInts: scalactic.Or[List[Int], ErrorMessage] = createListOfInts(testRow)

                assert(listOfInts.isBad)
            }
        }

        describe("splitTextIntoRows(stringOfInts:String): List[String]") {
            it("should return a list of strings") {
                val listOfStrings = splitTextIntoRows(testData)

                assert(listOfStrings.nonEmpty)
            }
        }

        describe("Calculate solution") {
            it("it should return the correct result") {
                val result: scalactic.Or[Int, ErrorMessage] = getSolutionFromLinesOfText(testData)

                assert(result.getOrElse(-1) == 36174)
            }
        }

        describe("getMultipleOfInt(num: Int, elementsToMatchAgainst: List[Int]) : Option[MultipleValues]")  {
            it ("it should find numbers that are multiples of each other") {
                val default = MultipleValues(-1, -1)
                val valuePair: MultipleValues = getMultipleOfInt(2, List(5, 9, 8)).getOrElse(default)

                assert(valuePair.higherVal == 8)
                assert(valuePair.lowerVal == 2)
            }
        }

        describe("getMultiplesInLine(listOfInt: List[Int]): Option[MultipleValues]") {
            it("should return the correct results for different values") {
                val default = MultipleValues(-1, -1)
                val valuePair: MultipleValues = getMultiplesInLine(List(5,9,2,8)).getOrElse(default)

                assert(valuePair.higherVal == 8)
                assert(valuePair.lowerVal == 2)

                val valuePair2: MultipleValues = getMultiplesInLine(List(9,4,7,3)).getOrElse(default)

                assert(valuePair2.higherVal == 9)
                assert(valuePair2.lowerVal == 3)

                val valuePair3: MultipleValues = getMultiplesInLine(List(3,8,6,5)).getOrElse(default)

                assert(valuePair3.higherVal == 6)
                assert(valuePair3.lowerVal == 3)
            }
        }

        describe("getCheckSumForMultiplesLine(line:List[Int]])") {
            it("should return the correct results for different values") {
                assert(getCheckSumForMultiplesLine(List(5,9,2,8)) == 4)

                assert(getCheckSumForMultiplesLine(List(9,4,7,3)) == 3)

                assert(getCheckSumForMultiplesLine(List(3,8,6,5)) == 2)

            }
        }

        describe("getCheckSumForMultiplesMatrix(matrix: List[List[Int]])") {
           it("should return the correct results for a value matrix") {
               assert(
                   getCheckSumForMultiplesMatrix(
                       List(
                           List(5,9,2,8),
                           List(9,4,7,3),
                           List(3,8,6,5)
                       )
                   ) == 9
               )
           }
        }

        describe("getCheckSumForMultiplesStringOfRows(stringOfRows:String): Int") {
            it("should return the correct result for the test data") {
                assert(getCheckSumForMultiplesStringOfRows(testData).getOrElse(-1) == 244)
            }
        }
    }
}

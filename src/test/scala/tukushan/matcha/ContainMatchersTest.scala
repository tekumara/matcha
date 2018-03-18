package tukushan.matcha

import java.sql.Timestamp

import org.scalatest.FunSuite

class ContainMatchersTest extends FunSuite with ContainMatchers {

  val row1 = Map(
    "int" -> 1,
    "float" -> 1.0F,
    "long" -> 1L,
    "string" -> "hi",
    "boolean" -> true,
    "timestamp" -> new Timestamp(0)
  )
  val row2 = Map(
    "int" -> 2,
    "float" -> 2.0F,
    "long" -> 2L,
    "string" -> "bye",
    "boolean" -> true,
    "timestamp" -> null
  )

  test("containInOrderOnly actual has more elements than expected") {
    val expected = Seq(row1)
    val actual = Seq(row1, row2)

    containInOrderOnly(expected).apply(actual).failureMessage should be (
      """expecting 1 results but got the following 2 results List(Map("timestamp" -> 1970-01-01 10:00:00.0, "float" -> 1.0, "string" -> "hi", "long" -> 1, "boolean" -> true, "int" -> 1), Map("timestamp" -> null, "float" -> 2.0, "string" -> "bye", "long" -> 2, "boolean" -> true, "int" -> 2))""".stripMargin
    )
  }

  test("containInOrderOnly missing") {
    val expected = Seq(row1)
    val actual = Seq(row2)

    containInOrderOnly(expected).apply(actual).failureMessage should be (
      """missing List(Map("timestamp" -> 1970-01-01 10:00:00.0, "float" -> 1.0, "string" -> "hi", "long" -> 1, "boolean" -> true, "int" -> 1)) from:
        |List(Map("timestamp" -> null, "float" -> 2.0, "string" -> "bye", "long" -> 2, "boolean" -> true, "int" -> 2))""".stripMargin
    )

    containInOrderOnly(expected).apply(Seq.empty).failureMessage should be (
      """missing List(Map("timestamp" -> 1970-01-01 10:00:00.0, "float" -> 1.0, "string" -> "hi", "long" -> 1, "boolean" -> true, "int" -> 1)) from:
        |List()""".stripMargin
    )

  }

  test("containInOrderOnly out of order") {
    val expected = Seq(row1, row2)
    val actual = Seq(row2, row1)

    containInOrderOnly(expected).apply(actual).failureMessage should be (
      """out of order List(Map("timestamp" -> 1970-01-01 10:00:00.0, "float" -> 1.0, "string" -> "hi", "long" -> 1, "boolean" -> true, "int" -> 1), Map("timestamp" -> null, "float" -> 2.0, "string" -> "bye", "long" -> 2, "boolean" -> true, "int" -> 2)) in:
        |List(Map("timestamp" -> null, "float" -> 2.0, "string" -> "bye", "long" -> 2, "boolean" -> true, "int" -> 2), Map("timestamp" -> 1970-01-01 10:00:00.0, "float" -> 1.0, "string" -> "hi", "long" -> 1, "boolean" -> true, "int" -> 1))""".stripMargin
    )
  }

}

package tukushan.matcha

import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.JavaConverters._

trait ContainMatchers extends Matchers {

  def containInOrderOnly[V <: Any](expected: Seq[Map[String, V]]) = if (expected.isEmpty) throw new Exception("No rows to test") else new ContainInOrderOnly(expected)

  // same as "theSameElementsInOrderAs" but nicer failure messaging. order does matter.
  class ContainInOrderOnly[V <: Any](expected: Seq[Map[String, V]]) extends Matcher[Seq[Map[String, Any]]] {

    override def apply(left: Seq[Map[String, Any]]): MatchResult = {

      if (left.nonEmpty && left.forall(_.isEmpty)) {
        MatchResult(
          matches = false,
          s"No results contained the keys ${expected.head.keySet.mkString(",")}. Did you specify a correct header?",
          "actual contains some of the keys"
        )
      } else if (left.size > expected.size) {
        MatchResult(
          matches = false,
          s"expecting ${expected.size} results but got the following ${left.size} results $left",
          "actual contains same number of results as expected"
        )
      }
      else {
        containsInOrder(left, expected)
      }
    }
  }


  def containOnly[V <: Any](expected: Seq[Map[String, V]]) = if (expected.isEmpty) throw new Exception("No rows to test") else new ContainOnly(expected)

  // converts j.u.Map to scala
  def containOnlyJavaMaps[V <: Any](expected: Seq[java.util.Map[String, V]]) = if (expected.isEmpty) throw new Exception("No rows to test") else new ContainOnly(expected.map(_.asScala.toMap))

  // same as "contain theSameElementsAs" but nicer failure messaging. order doesn't matter.
  class ContainOnly[V <: Any](expected: Seq[Map[String, V]]) extends Matcher[Seq[Map[String, Any]]] {

    override def apply(left: Seq[Map[String, Any]]): MatchResult = {

      if (left.nonEmpty && left.forall(_.isEmpty)) {
        MatchResult(
          matches = false,
          s"No results contained the keys ${expected.head.keySet.mkString(",")}. Did you specify a correct header?",
          "actual contains some of the keys"
        )
      } else if (left.size > expected.size) {
        MatchResult(
          matches = false,
          s"expecting ${expected.size} results but got the following ${left.size} results: \n$left",
          "actual contains same number of results as expected"
        )
      }
      else {
        contains(left, expected)
      }
    }
  }

  def contain[V <: Any](expected: Seq[Map[String, V]]) = if (expected.isEmpty) throw new Exception("No rows to test") else new ContainAtLeast(expected)

  // same as "contain" but nicer failure messaging. order doesn't matter.
  class ContainAtLeast[V <: Any](expected: Seq[Map[String, V]]) extends Matcher[Seq[Map[String, Any]]] {
    override def apply(left: Seq[Map[String, Any]]): MatchResult = {

      if (left.nonEmpty && left.forall(_.isEmpty)) {
        MatchResult(
          matches = false,
          s"No results contained the keys ${expected.head.keySet.mkString(",")}. Did you specify a header?",
          "actual contains some of the keys"
        )
      }
      else {
        contains(left, expected)
      }
    }
  }

  def contains[V <: Any](left: Seq[Map[String, Any]], expected: Seq[Map[String, V]]) = {
    val missing = expected.flatMap(row => {
      if (left.contains(row)) {
        None
      } else {
        Some(row.toString())
      }
    })

    MatchResult(
      missing.isEmpty,
      s"missing ${missing.mkString(", ")} from:\n$left",
      "actual contains all key/value pairs"
    )
  }

  def containsInOrder[V <: Any](left: Seq[Map[String, Any]], expected: Seq[Map[String, V]]) = {
    val missing = expected.flatMap(row => {
      if (left.contains(row)) {
        None
      } else {
        Some(row.toString())
      }
    })

    if (missing.nonEmpty) {
      MatchResult(
        matches = false,
        s"missing ${missing.mkString(", ")} from:\n$left",
        "actual contains all key/value pairs"
      )
    } else {
      val outOfOrder: Seq[Map[String, V]] = expected.zip(left).filter{ case (expectation, actual) => expectation != actual }.map{ case (expectation, actual) => expectation }

      MatchResult(
        matches = outOfOrder.isEmpty,
        s"out of order ${outOfOrder.mkString(", ")} in:\n$left",
        "actual contains all key/value pairs"
      )
    }

  }


}

package tukushan.matcha

import org.scalatest.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.JavaConverters._

trait ContainMatchers extends Matchers {

  def containInOrderOnly[V <: Any](expected: Seq[Map[String, V]]) = if (expected.isEmpty) throw new Exception("No rows to test") else new ContainInOrderOnly(expected)

  // same as "contain theSameElementsInOrderAs" but nicer failure messaging. order does matter.
  class ContainInOrderOnly[V <: Any](expected: Seq[Map[String, V]]) extends Matcher[Seq[Map[String, Any]]] {

    override def apply(left: Seq[Map[String, Any]]): MatchResult = {

      if (left.nonEmpty && left.forall(_.isEmpty)) {
        MatchResult(
          matches = false,
          rawFailureMessage = s"No results contained the keys {0}. Did you specify a correct header?",
          rawNegatedFailureMessage = "actual contains some of the keys",
          args = Vector(expected.head.keySet.mkString(","))
        )
      } else if (left.size > expected.size) {
        MatchResult(
          matches = false,
          rawFailureMessage = s"expecting {0} results but got the following {1} results {2}",
          rawNegatedFailureMessage = "actual contains same number of results as expected",
          args = Vector(expected.size, left.size, left)
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
          rawFailureMessage = s"No results contained the keys {0}. Did you specify a correct header?",
          rawNegatedFailureMessage = "actual contains some of the keys",
          args = Vector(expected.head.keySet.mkString(","))
        )
      } else if (left.size > expected.size) {
        MatchResult(
          matches = false,
          rawFailureMessage = s"expecting {0} results but got the following {1} results {2}",
          rawNegatedFailureMessage = "actual contains same number of results as expected",
          args = Vector(expected.size, left.size, left)
        )
      }
      else {
        contains(left, expected)
      }
    }
  }

  def containAtLeast[V <: Any](expected: Seq[Map[String, V]]) = if (expected.isEmpty) throw new Exception("No rows to test") else new ContainAtLeast(expected)

  // same as "contain" but nicer failure messaging. order doesn't matter.
  class ContainAtLeast[V <: Any](expected: Seq[Map[String, V]]) extends Matcher[Seq[Map[String, Any]]] {
    override def apply(left: Seq[Map[String, Any]]): MatchResult = {

      if (left.nonEmpty && left.forall(_.isEmpty)) {
        MatchResult(
          matches = false,
          rawFailureMessage = s"No results contained the keys {0}. Did you specify a correct header?",
          rawNegatedFailureMessage = "actual contains some of the keys",
          args = Vector(expected.head.keySet.mkString(","))
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
        Some(row)
      }
    })

    MatchResult(
      matches = missing.isEmpty,
      rawFailureMessage = s"missing {0} from:\n{1}",
      rawNegatedFailureMessage = "actual contains all key/value pairs",
      args = Vector(missing, left)
    )
  }

  def containsInOrder[V <: Any](left: Seq[Map[String, Any]], expected: Seq[Map[String, V]]) = {
    val missing = expected.flatMap(row => {
      if (left.contains(row)) {
        None
      } else {
        Some(row)
      }
    })

    if (missing.nonEmpty) {
      MatchResult(
        matches = false,
        rawFailureMessage = s"missing {0} from:\n{1}",
        rawNegatedFailureMessage = "actual contains all key/value pairs",
        args = Vector(missing, left)
      )
    } else {
      val outOfOrder: Seq[Map[String, V]] = expected.zip(left).filter{ case (expectation, actual) => expectation != actual }.map{ case (expectation, _) => expectation }

      MatchResult(
        matches = outOfOrder.isEmpty,
        rawFailureMessage = s"out of order {0} in:\n{1}",
        rawNegatedFailureMessage = "actual contains all key/value pairs",
        args = Vector(outOfOrder, left)
      )
    }

  }

}

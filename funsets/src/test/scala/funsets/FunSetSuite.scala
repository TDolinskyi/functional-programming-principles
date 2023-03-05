package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains elements within both sets") {
    new TestSets {
      val s = union(s1, s2)
      val i = intersect(s, s1)
      assert(contains(i, 1), "Intersect 1")
      assert(!contains(i, 2), "Intersect 2")
      assert(!contains(i, 3), "Intersect 3")
    }
  }

  test("filter of {1,3,4,5,7,1000} for _ < 5") {
    new TestSets {
      assert(contains(filter(fromListToSet(List(1, 3, 4, 5, 7, 1000)), _ < 5), 1))
    }
  }

  test("forall returns true for predicate true") {
    new TestSets {
      val setWithOnlyOdds = union(s1, s3)
      val setWithEvens = union(s1, s2)
      assert(forall(setWithOnlyOdds, _ > 0))
      assert(!forall(setWithEvens, _ > 3))
    }
  }

  test("diff of {1,3,4,5,7,1000} and {1,2,3,4}") {
    new TestSets {
      val firstSet = fromListToSet(List(1,3,4,5,7,1000))
      val secondSet = fromListToSet(List(1,2,3,4))
      val result = fromListToSet(List(5,7,1000))
      assert(FunSets.toString(diff(firstSet, secondSet)) == FunSets.toString(result))
    }
  }

  test("exists returns true for predicate true") {
    new TestSets {
      val setWithOnlyOdds = union(s1, s3)
      val setWithEvensAndOdds = union(s1, s2)
      assert(exists(setWithOnlyOdds, _ - 1 == 2))
      assert(exists(setWithEvensAndOdds, _ - 1 == 1))
      assert(!exists(s2, _ > 3))
    }
  }

  test("map applies function over all elements") {
    new TestSets {
      val s = union(s1, union(s2, s3))
      val mappedSet = map(s, _ + 1)
      assert(contains(mappedSet, 2))
      assert(contains(mappedSet, 3))
      assert(contains(mappedSet, 4))
    }
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds

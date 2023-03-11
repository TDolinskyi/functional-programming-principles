package patmat

class HuffmanSuite extends munit.FunSuite:

  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Leaf('u', 2)
    val t4 = Fork(Fork(Fork(Leaf('T', 1), Leaf('t', 1), List('T', 't'), 2), Leaf('e', 2), List('T', 't', 'e'), 4), Fork(Fork(Leaf('s', 1), Leaf('x', 1), List('s', 'x'), 2), Fork(Leaf('m', 1), Leaf('o', 1), List('m', 'o'), 2), List('s', 'x', 'm', 'o'), 4), List('T', 't', 'e', 's', 'x', 'm', 'o'), 8)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("chars of a simple tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t3), List('u'))
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a', 'b', 'd'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("decode and encode a very short text with code table (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, quickEncode(t1)("ab".toList)), "ab".toList)
  }

  test("gives an optimal code tree, the number of bits when encoding 'someText' is minimal (15pts)") {
    new TestTrees:
      assertEquals(createCodeTree("someText".toList), t4)
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds

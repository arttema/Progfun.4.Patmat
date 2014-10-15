package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("sample make tree") {
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    )

    val content: Boolean = sampleTree.chars == List('x', 'e', 't')
    val weight: Boolean = sampleTree.weight == 4
    assert(content && weight)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    val chars: List[Char] = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    assert(string2Chars("hello, world") === chars)
  }

  test("times") {
    val chars: List[Char] = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    val times1: List[(Char, Int)] = times(chars)
  }

  test("Ordered list") {
    val chars: List[Char] = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    val ordered = makeOrderedLeafList(times(chars))
    assert(ordered.head.weight < ordered.last.weight)
  }

  test("Secret decode") {
    val decodedSecret: List[Char] = decode(frenchCode, secret)
    assert("huffmanestcool" == (decodedSecret mkString) )
  }


  test("Convert code tree to code table") {
    println(convert(frenchCode))
  }


  test("Encode using tree") {
    val decodedSecret: List[Char] = decode(frenchCode, secret)
    decodedSecret.foreach(print)

    val encode1: List[Bit] = encode(frenchCode)(decodedSecret)
    assert(encode1 == secret)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}

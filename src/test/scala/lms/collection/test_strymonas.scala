package lms.collection

import lms.TutorialFunSuite
import lms.core.stub.{DslDriver, DslGen}
import lms.core.virtualize
import lms.macros.SourceContext

class Strym extends TutorialFunSuite {
  val under = ""

  abstract class DslDriverStreams[A: Manifest, B: Manifest] extends DslDriver[A,B] with StreamDSL {q =>
    override val codegen = new DslGen with StreamDSL {
      val IR: q.type = q
    }

    def sum[T:Numeric:Manifest] = (a: Rep[T]) => (e: Rep[T]) => a + e
    def mul[T:Numeric:Manifest] = (a: Rep[T]) => (e: Rep[T]) => a * e

    def arange(l: Rep[Int]): Rep[Array[Int]] = {
      val a1 = NewArray[Int](l)
      for (i <- 0 until l: Rep[Range]) {
        a1(i) = i
      }
      a1
    }
  }

  test("dotProd") {
    val dotProd = new DslDriverStreams[Int, Int] {
      @virtualize
      def snippet(len: Rep[Int]): Rep[Int] = {
        val a1 = arange(len)
        Stream(a1).zip(mul, Stream(a1)).fold(0, sum)
      }
    }

    assert(dotProd.eval(10) == 285)
  }

  test("contrived") {

    val contrived = new DslDriverStreams[Int, Unit] {
      @virtualize
      def length(len: Rep[Int]): Rep[Unit] = {
        val s1 = Stream(NewArray[Int](len))
        val streams = s1.map(l => NewArray[Int](l))
        val inp = streams.materialize(len)

        val maxLen: Rep[Array[Array[Int]] => Int] = fun { (a: Rep[Array[Array[Int]]]) =>
          var max = a(0).length
          for (i <- (0 until a.length) : Rep[Range]) {
            if (a(i).length > max) max = a(i).length
          }
          readVar(max)
        }
        val res = Stream(inp: Rep[Array[Array[Int]]]).filter(a =>
          Stream(a).fold(0, sum[Int]) > maxLen(inp) / 2
        )

        val fin = res.fold(0, (a: Rep[Int]) => (e: Rep[Array[Int]]) =>
          a + Stream(e).fold(0, sum[Int])
        )
        println(fin)
      }

      @virtualize
      def snippet(x: Rep[Int]) = length(x)
    }
    println(contrived.code)
  }
}

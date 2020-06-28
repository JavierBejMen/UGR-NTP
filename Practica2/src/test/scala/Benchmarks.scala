import org.scalameter.api._

object Benchmarks extends Bench.LocalTime {
  val sizes = Gen.range("size")(1, 10, 1)

  val r = scala.util.Random

  val ranges = for {
    size <- sizes
  } yield r.nextInt(999) until size

  performance of "GenericBinarySearch" in {
    measure method "calc" in {
      using(ranges) in{
        GenericBinarySearch.calc[Int](Seq.fill(100 + r.nextInt((1000-100)+1)), r.nextInt(999), (x: Int, y: Int) => x < y)
      }
    }
  }
}

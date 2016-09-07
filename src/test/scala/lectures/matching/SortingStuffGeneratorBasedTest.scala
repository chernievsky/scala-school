package lectures.matching

import lectures.matching.SortingStuff.{Book, StuffBox, Watches, Knife, Boots, findMyKnife}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то  метод findMyKnife его отыщет
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen = bookGenerator.filter(_.isInteresting)
  val knifeGen = Gen.option(Knife)
  val adidasConverseBootsGen: Gen[Boots] = Gen.zip(Gen.oneOf("Adidas", "Converse"), Gen.choose(40, 45)).map(b => Boots(b._1, b._2))
  val otherBootsGen: Gen[Boots] = Gen.zip(Gen.oneOf("Nike", "Reebok", "New Balance"), Gen.choose(40, 45)).map(b => Boots(b._1, b._2))

  // Override configuration if you need
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 10, maxSize = 20)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }
        val boots = (1 to ms) flatMap { _ => otherBootsGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches ++ boots).toList)
        goodBooks should have size books.size
        niceWatches should have size 0
        junk should have size watches.size + boots.size
      }
    }
    "find knife" which {
      "was occasionally disposed" in {
        val testKnife = knifeGen.sample
        testKnife match {
          case Some(Some(k)) =>
            val stuffBox = SortingStuff.sortJunk(List(k))
            findMyKnife(stuffBox) shouldBe true
          case _ =>
            val stuffBox = SortingStuff.sortJunk(List())
            findMyKnife(stuffBox) shouldBe false
        }
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in {
        val ms = generatorDrivenConfig.minSuccessful

        val adidasConverseBoots = (1 to ms) flatMap { _ => adidasConverseBootsGen.sample }
        val StuffBox(_, _, boots, _) = SortingStuff.sortJunk(Random.shuffle(adidasConverseBoots).toList)
        boots should have size adidasConverseBoots.size
      }
    }
  }
}
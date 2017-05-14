package lectures.functions

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

import Authentication._
import AuthenticationData._

/**
  * Авторизация - это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec with Matchers with PropertyChecks {
  val validCardUsers: Gen[CardUser] =
    Gen.zip(Gen.choose(0, 1000), Gen.oneOf(registeredCards.toList)).map(gen => CardUser(gen._1, gen._2))

  val invalidCardUsers: Gen[CardUser] =
    Gen.zip(Gen.choose(0, 1000),
            Gen.choose(0, 1000).map(CardCredentials(_)).filter(!registeredCards.contains(_)))
       .map(gen => CardUser(gen._1, gen._2))

  val validLPUsers: Gen[LPUser] =
    Gen.zip(Gen.choose(0, 1000), Gen.oneOf(registeredLoginAndPassword.toList)).map(gen => LPUser(gen._1, gen._2))

  val invalidLPUsers: Gen[LPUser] =
    Gen.zip(Gen.choose(0, 1000), Gen.alphaStr, Gen.alphaStr)
      .map(gen => LPUser(gen._1, LPCredentials(gen._2, gen._3)))
      .filter(u => !registeredLoginAndPassword.contains(u.credentials))


  "Card user" when {
    "has valid card" should {
      "be authenticated" in {
        forAll(validCardUsers) {
          user => authByCard.isDefinedAt(user) shouldBe true
        }
      }
    }
    "has invalid card" should {
      "not be authenticated" in {
        forAll(invalidCardUsers) {
          user => authByCard.isDefinedAt(user) shouldBe false
        }
      }
    }
  }

  "LoginPassword user" when {
    "has valid login and password" should {
      "be authenticated" in {
        forAll(validLPUsers) {
          user => authByLP.isDefinedAt(user) shouldBe true
        }
      }
    }
    "has invalid login and/or password" should {
      "not be authenticated" in {
        forAll(invalidLPUsers) {
          user => authByLP.isDefinedAt(user) shouldBe false
        }
      }
    }
  }

  "User" when {
    "has invalid login and password but has valid card" should {
      "be authenticated" in {
        forAll(Gen.zip(invalidLPUsers, validCardUsers)) {
          user => (authByLP.lift(user._1) orElse authByCard.lift(user._2)).isDefined shouldBe true
        }
      }
    }
  }
}

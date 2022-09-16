package traffic.main

import munit.CatsEffectSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import traffic.Generators

class MainSuite extends CatsEffectSuite with ScalaCheckDrivenPropertyChecks with Generators {
  import Main._
  test("validateIntersectionFormat should be valid for correct format") {
    forAll(validIntersectionStringGen) { (s: String) =>
      assert(validateIntersectionFormat(s).isValid)
    }
  }

  test("validateIntersectionFormat should be invalid for incorrect formats") {
    forAll(invalidIntersectionStringGen) { (s: String) =>
      assert(validateIntersectionFormat(s).isInvalid)
    }
  }

}

package traffic

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite

class HelloWorldSuite extends CatsEffectSuite {

  test("test hello world says hi") {
    IO("Hello Cats!").map(it => assertEquals(it, "Hello Cats!"))
  }
}

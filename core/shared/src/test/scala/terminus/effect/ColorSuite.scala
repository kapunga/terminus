/*
 * Copyright 2024 Creative Scala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package terminus.effect

import munit.FunSuite
import terminus.StringBuilderTerminal

class ColorSuite extends FunSuite {
  test(
    "Foreground color code reverts to enclosing color after leaving inner colored block"
  ) {
    val result =
      StringBuilderTerminal.run { t ?=>
        t.foreground.blue {
          t.write("Blue ")
          t.foreground.red { t.write("Red ") }
          t.write("Blue ")
        }
      }

    assertEquals(
      result,
      s"${AnsiCodes.foreground.blue}Blue ${AnsiCodes.foreground.red}Red ${AnsiCodes.foreground.blue}Blue ${AnsiCodes.foreground.default}"
    )
  }
}

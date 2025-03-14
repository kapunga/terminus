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

package terminus

trait Terminal
    extends effect.AlternateScreenMode[Terminal],
      effect.ApplicationMode[Terminal],
      effect.Color[Terminal],
      effect.Cursor,
      effect.Format[Terminal],
      effect.Erase,
      effect.KeyReader,
      effect.NonBlockingReader,
      effect.RawMode[Terminal],
      effect.Reader,
      effect.Writer
type Program[A] = Terminal ?=> A

object Terminal
    extends AlternateScreenMode,
      ApplicationMode,
      Color,
      Cursor,
      Format,
      Erase,
      KeyReader,
      NonBlockingReader,
      Peeker,
      RawMode,
      Reader,
      Writer {

  def run[A](f: Program[A]): A = {
    val terminal = NativeTerminal
    val result = f(using terminal)

    result
  }
}

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

import cats.effect.{Resource, Sync}
import scala.language.postfixOps

extension (terminal: Terminal)
  def inAlternateScreenMode[F[_]: Sync]: Resource[F, Unit] = inMode(
    _.setAlternateScreenMode()
  )

  def inApplicationMode[F[_]: Sync]: Resource[F, Unit] = inMode(
    _.setApplicationMode()
  )

  def inRawMode[F[_]: Sync]: Resource[F, Unit] = inMode(_.setRawMode())

  private def inMode[F[_]](
      setMode: Terminal => () => Unit
  )(using mc: Sync[F]): Resource[F, Unit] =
    Resource
      .make(Sync[F].delay(setMode(terminal)))(r => mc.delay(r()))
      .map(_ => ())

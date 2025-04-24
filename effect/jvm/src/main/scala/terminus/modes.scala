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

import cats.effect.{MonadCancel, Resource}
import scala.language.postfixOps

extension (terminal: Terminal)
  def inAlternateScreenMode[A, F[_]](fa: F[A])(using
      mc: MonadCancel[F, Throwable]
  ): F[A] =
    inMode(_.setAlternateScreenMode())(fa)

  def inApplicationMode[A, F[_]](fa: F[A])(using
      mc: MonadCancel[F, Throwable]
  ): F[A] =
    inMode(_.setApplicationMode())(fa)

  def inRawMode[A, F[_]](fa: F[A])(using mc: MonadCancel[F, Throwable]): F[A] =
    inMode(_.setRawMode())(fa)

  private def inMode[A, F[_]](
      setMode: Terminal => () => Unit
  )(fa: F[A])(using mc: MonadCancel[F, Throwable]): F[A] =
    Resource
      .make(mc.pure(setMode(terminal)))(revert => mc.pure(revert()))
      .use(_ => fa)

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

import cats.data.NonEmptyList
import cats.effect.{Async, Clock}
import fs2.{Chunk, Pull, Stream}
import java.time.Instant
import scala.language.postfixOps

object KeyStream:
  def apply[F[_]: Async]: Stream[F, Key] =
    val inputStream: Stream[F, Char] =
      fs2.io.stdinUtf8(16).flatMap(s => Stream.emits(s.toList))

    mapRoot(inputStream.pull.uncons1).stream

  private def mapRoot[F[_]: Async](
      pull: Pull[F, Nothing, Option[(Char, Stream[F, Char])]]
  ): Pull[F, Key, Nothing] = {
    pull.flatMap {
      case None =>
        Pull.raiseError(
          new RuntimeException("Unexpected termination of stdin.")
        )
      case Some((c, rest)) =>
        KeyMappings.default.getOrElse(c, Key(c)) match
          case k: Key          => Pull.output1(k) >> mapRoot(rest.pull.uncons1)
          case ks: KeySequence => mapKeySequence(c, ks, rest)
    }
  }

  private def mapKeySequence[F[_]: Async](
      h: Char,
      ks: KeySequence,
      rest: Stream[F, Char]
  ): Pull[F, Key, Nothing] = {
    def timeoutPull: Pull[F, Nothing, Instant] =
      Pull.eval(Clock[F].realTimeInstant).map(_.plusMillis(100))

    def go(
        sequence: String,
        keys: List[Key],
        stream: Stream[F, Char],
        timeoutAfter: Instant
    ): Pull[F, Key, Nothing] = {
      Pull
        .eval(Clock[F].realTimeInstant)
        .flatMap(now => {
          val expired = now.isAfter(timeoutAfter)

          if expired then
            Pull.output(Chunk.from(keys)) >> mapRoot(stream.pull.uncons1)
          else {
            stream.pull.uncons1.flatMap {
              case None =>
                Pull.output(Chunk.from(keys)) >> Pull.raiseError(
                  new RuntimeException("Unexpected termination of stdin.")
                )
              case Some((c, s)) =>
                val newSeq = sequence.appended(c)
                val nextKey = KeyMappings.default.getOrElse(c, Key(c))

                nextKey match {
                  case ks: KeySequence =>
                    Pull.output(Chunk.from(keys)) >>
                      timeoutPull
                        .flatMap(ta => go(c.toString, List(ks.root), s, ta))
                  case k: Key =>
                    ks.isKeySequence(newSeq) match
                      case IsKeySequence.No =>
                        Pull.output(Chunk.from(keys.appended(k))) >> mapRoot(
                          s.pull.uncons1
                        )
                      case IsKeySequence.Maybe =>
                        go(newSeq, keys.appended(k), s, timeoutAfter)
                      case IsKeySequence.Yes(key) =>
                        Pull.output1(key) >> mapRoot(s.pull.uncons1)
                }
            }
          }
        })
    }

    timeoutPull.flatMap(timeout => go(h.toString, List(ks.root), rest, timeout))
  }

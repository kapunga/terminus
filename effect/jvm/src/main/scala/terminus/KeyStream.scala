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

import cats.effect.{Async, Clock}
import fs2.{Chunk, Pull, Stream}
import java.time.Instant
import scala.language.postfixOps

object KeyStream:
  private def stringToCharPipe[F[_]](s: Stream[F, String]): Stream[F, Char] =
    s.flatMap(s => Stream.emits(s.toList))

  private def toKeyPipe[F[_]: Async](
      inputStream: Stream[F, Char]
  ): Stream[F, Key] =
    toKeyPull(inputStream).stream

  def apply[F[_]: Async]: Stream[F, Key] =
    fs2.io
      .stdinUtf8(32)
      .through(stringToCharPipe)
      .through(toKeyPipe)

  private def toKeyPull[F[_]: Async](
      stream: Stream[F, Char]
  ): Pull[F, Key, Nothing] = {
    stream.pull.uncons1.flatMap {
      case None =>
        Pull.raiseError(
          new RuntimeException("Unexpected termination of stdin.")
        )
      case Some((c, rest)) =>
        KeyMappings.default.getOrElse(c, Key(c)) match
          case k: Key          => Pull.output1(k) >> toKeyPull(rest)
          case ks: KeySequence => readKeySequence(c, ks, rest)
    }
  }

  private def readKeySequence[F[_]: Async](
      h: Char,
      ks: KeySequence,
      rest: Stream[F, Char]
  ): Pull[F, Key, Nothing] = {
    def currentPull: Pull[F, Nothing, Instant] =
      Pull.eval(Clock[F].realTimeInstant)
    def timeoutPull: Pull[F, Nothing, Instant] =
      currentPull.map(_.plusMillis(100))

    def go(
        sequence: String,
        keys: List[Key],
        stream: Stream[F, Char],
        timeoutAfter: Instant
    ): Pull[F, Key, Nothing] = {
      currentPull
        .flatMap(now => {
          if now.isAfter(timeoutAfter) then
            Pull.output(Chunk.from(keys)) >> toKeyPull(stream)
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
                  case newKs: KeySequence =>
                    Pull
                      .output(Chunk.from(keys)) >> readKeySequence(c, newKs, s)
                  case k: Key =>
                    ks.isKeySequence(newSeq) match
                      case IsKeySequence.No =>
                        Pull.output(Chunk.from(keys.appended(k))) >> toKeyPull(
                          s
                        )
                      case IsKeySequence.Maybe =>
                        go(newSeq, keys.appended(k), s, timeoutAfter)
                      case IsKeySequence.Yes(key) =>
                        Pull.output1(key) >> toKeyPull(s)
                }
            }
          }
        })
    }

    timeoutPull.flatMap(timeout => go(h.toString, List(ks.root), rest, timeout))
  }

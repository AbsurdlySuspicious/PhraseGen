/*
 *    Copyright 2019 AbsurdlySuspicious
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package phgen

import scala.collection.mutable
import scala.util.Random
import scala.util.matching.Regex

object Utils {

  def matcher(r: Regex) = (in: String) => r.pattern.matcher(in).matches

  def time = System.currentTimeMillis()

  class RandomIter[T](src: Seq[T]) extends Iterator[T] {
    val rnd = new Random
    val max = src.length

    override def hasNext   = true
    override def next(): T = src(rnd.nextInt(max))
  }

}

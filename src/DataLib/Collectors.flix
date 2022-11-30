/*
 * Copyright 2022 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */



namespace DataLib/Collectors {

    /// TODO - these should be in flix-sandbox as they are experimental...

    /// Can we provide all / most of Java's Collectors using Foldable?

    // At the moment use horrible casts to IO so we can reify the effect `ef` in Collector's signature.
    type alias Collector[a: Type, acc: Type, ans: Type, ef: Bool] = 
        { supplier      = Unit -> acc \ IO
        , accumulator   = a -> acc -> acc \ IO
        , finisher      = acc -> ans \ IO
        }

    pub def collect(c: Collector[a, acc, ans, ef], t: t[a]): ans \ ef with Foldable[t] = 
        let st = (c.supplier)() as \ ef; 
        ((Foldable.foldLeft(flip(c.accumulator), st, t) as \ ef)|> c.finisher) as \ ef

    pub def averagingInt32(f: a -> Int32 \ ef): Collector[a, (Float64, Float64), Float64, ef] = 
        { supplier      = () -> (0.0f64, 0.0f64) as \ IO
        , accumulator   = (a, ac) -> {let (n, tot) = ac; let i = f(a) as \ IO; (n + 1.0f64, tot + Int32.toFloat64(i))} 
        , finisher      = match (n, tot) -> tot / n as \ IO
        }

    pub def averagingInt64(f: a -> Int64 \ ef, t: t[a]): Float64 \ ef with Foldable[t] = 
        Foldable.foldLeft((ac, a) -> {let (n, tot) = ac; let i = f(a); (n + 1.0f64, tot + Int64.toFloat64(i))}, (0.0f64, 0.0f64), t)
            |> match(n, tot) -> tot / n

    pub def averagingFloat64(f: a -> Float64 \ ef, t: t[a]): Float64 \ ef with Foldable[t] = 
        Foldable.foldLeft((ac, a) -> {let (n, tot) = ac; (n + 1.0f64, tot + f(a))}, (0.0f64, 0.0f64), t)
            |> match(n, tot) -> tot / n

    /// `filtering` is a "collector transformer" it is not possible to implement if we don't have a tangible collector
    pub def filtering(f: a -> Bool \ ef1, c: Collector[a, acc, ans, ef]): Collector[a, acc, ans, ef and ef1] = 
        let accum1 = (a, acc) -> if (f(a)) (c.accumulator)(a, acc) else acc;
        { supplier = c.supplier 
        , accumulator = accum1
        , finisher = c.finisher
        }

}

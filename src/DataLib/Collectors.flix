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

    use DataLib.Collector

    pub def averagingInt32(f: a -> Int32 \ ef): Collector[a, (Float64, Float64), Float64, ef] = 
        { supplier      = () -> (0.0f64, 0.0f64) as \ ef
        , accumulator   = (a, acc) -> {let (n, tot) = acc; let i = f(a); (n + 1.0f64, tot + Int32.toFloat64(i))} 
        , finisher      = match (n, tot) -> tot / n as \ ef
        }

    pub def averagingInt64(f: a -> Int64 \ ef): Collector[a, (Float64, Float64), Float64, ef]  = 
        { supplier      = () -> (0.0f64, 0.0f64) as \ ef
        , accumulator   = (a, acc) -> {let (n, tot) = acc; let i = f(a); (n + 1.0f64, tot + Int64.toFloat64(i))}
        , finisher      = match(n, tot) -> tot / n as \ ef
        }

    pub def averagingFloat64(f: a -> Float64 \ ef): Collector[a, (Float64, Float64), Float64, ef] = 
        { supplier      = () -> (0.0f64, 0.0f64) as \ ef
        , accumulator   = (a, acs) -> {let (n, tot) = acs; (n + 1.0f64, tot + f(a))}
        , finisher      = match(n, tot) -> tot / n as \ ef
        }

    /// `filtering` is a "collector transformer" it is not possible to implement if we don't have a tangible collector
    pub def filtering(f: a -> Bool \ ef1, c: Collector[a, acc, ans, ef]): Collector[a, acc, ans, ef and ef1] = 
        let accum1      = (a, acc) -> if (f(a)) (c.accumulator)(a, acc) else acc;
        { supplier      = () -> (c.supplier)() as \ {ef, ef1} 
        , accumulator   = accum1
        , finisher      = acc -> (c.finisher)(acc) as \ {ef, ef1}
        }


    /// `andThen` is a "collector transformer" 
    pub def andThen(f: ans -> ans1 \ ef1, c: Collector[a, acc, ans, ef]): Collector[a, acc, ans1, ef and ef1] = 
        let finish1     = ans -> {let g = c.finisher; g(ans) |> f};
        { supplier      = () -> (c.supplier)() as \ {ef, ef1} 
        , accumulator   = (a, acc) -> (c.accumulator)(a, acc) as \ {ef, ef1}
        , finisher      = finish1
        }

    /// Foldable.sum
    pub def counting(): Collector[a, Int64, Int64, Pure] = 
        { supplier      = _ -> 0i64
        , accumulator   = (_, y) -> y + 1i64
        , finisher      = x -> x
        }

    /// Foldable.toList
    pub def toList(): Collector[a, List[a], List[a], Pure] = 
        { supplier      = _ -> Nil
        , accumulator   = (x, xs) -> x :: xs
        , finisher      = xs -> xs
        }

    /// Foldable.toSet
    pub def toSet(): Collector[a, Set[a], Set[a], Pure] with Order[a] = 
        { supplier      = _ -> Set.empty()
        , accumulator   = (x, s) -> Set.insert(x, s)
        , finisher      = s -> s
        }

    pub def groupingBy(f: a -> k \ ef): Collector[a, Map[k, List[a]], Map[k, List[a]], ef] with Order[k] = 
        { supplier      = () -> Map.empty() as \ ef
        , accumulator   = (a, acc) -> {let k = f(a); Map.insertWith((l1, l2) -> l1 ::: l2, k, a :: Nil, acc)}
        , finisher      = m -> m as \ ef
        }

}

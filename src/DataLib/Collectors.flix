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
        { supplier      = () -> unsafe_cast (0.0f64, 0.0f64) as _ \ ef
        , accumulator   = (a, acc) -> {let (n, tot) = acc; let i = f(a); (n + 1.0f64, tot + Int32.toFloat64(i))} 
        , finisher      = match (n, tot) -> unsafe_cast (tot / n) as _ \ ef
        }

    pub def averagingInt64(f: a -> Int64 \ ef): Collector[a, (Float64, Float64), Float64, ef]  = 
        { supplier      = () -> unsafe_cast (0.0f64, 0.0f64) as _ \ ef
        , accumulator   = (a, acc) -> {let (n, tot) = acc; let i = f(a); (n + 1.0f64, tot + Int64.toFloat64(i))}
        , finisher      = match(n, tot) -> unsafe_cast (tot / n) as _ \ ef
        }

    pub def averagingFloat64(f: a -> Float64 \ ef): Collector[a, (Float64, Float64), Float64, ef] = 
        { supplier      = () -> unsafe_cast (0.0f64, 0.0f64) as _ \ ef
        , accumulator   = (a, acs) -> {let (n, tot) = acs; (n + 1.0f64, tot + f(a))}
        , finisher      = match(n, tot) -> unsafe_cast (tot / n) as _ \ ef
        }

    /// `filtering` is a "collector transformer" it is not possible to implement if we don't have a tangible collector
    pub def filtering(f: a -> Bool \ ef1, c: Collector[a, acc, ans, ef]): Collector[a, acc, ans, ef and ef1] = 
        let accum1      = (a, acc) -> if (f(a)) (c.accumulator)(a, acc) else acc;
        { supplier      = () -> unsafe_cast ((c.supplier)()) as _ \ {ef, ef1} 
        , accumulator   = accum1
        , finisher      = acc -> unsafe_cast ((c.finisher)(acc)) as _ \ {ef, ef1}
        }


    /// `andThen` is a "collector transformer" 
    pub def andThen(f: ans -> ans1 \ ef1, c: Collector[a, acc, ans, ef]): Collector[a, acc, ans1, ef and ef1] = 
        let finish1     = ans -> {let g = c.finisher; g(ans) |> f};
        { supplier      = () -> unsafe_cast ((c.supplier)()) as _ \ {ef, ef1} 
        , accumulator   = (a, acc) -> unsafe_cast ((c.accumulator)(a, acc)) as _ \ {ef, ef1}
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
        { supplier      = () -> unsafe_cast Map.empty() as _ \ ef
        , accumulator   = (a, acc) -> {let k = f(a); Map.insertWith((l1, l2) -> l1 ::: l2, k, a :: Nil, acc)}
        , finisher      = m -> unsafe_cast m as _ \ ef
        }

    pub def collectListOpt(): Collector[Option[a], Option[List[a]], Option[List[a]], Pure] = 
        { supplier      = () -> Some(Nil)
        , accumulator   = (a, acc) -> 
            match (a, acc) {
                case (_, None)              => None
                case (None, _)              => None
                case (Some(a1), Some(acc1)) => Some(a1 :: acc1)
            }
        , finisher      = opt -> opt
        }

    pub def collectListRes(): Collector[Result[e, a], Result[e, List[a]], Result[e, List[a]], Pure] = 
        { supplier      = () -> Ok(Nil)
        , accumulator   = (a, acc) -> 
            match (a, acc) {
                case (_, Err(e))            => Err(e)
                case (Err(e), _)            => Err(e)
                case (Ok(a1), Ok(acc1))     => Ok(a1 :: acc1)
            }
        , finisher      = res -> res
        }
}

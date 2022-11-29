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


namespace DataLib {
    
    type alias Collector[a: Type, c: Type, ans: Type] = 
        { supplier      = Unit -> c
        , accumulator   = a -> c -> c
        , combiner      = c -> c -> c
        , finisher      = c -> ans
        }
}

namespace DataLib/Collector {

    use DataLib.Collector

    pub def counting(): Collector[a, Int64, Int64] = 
        { supplier      = _ -> 0i64
        , accumulator   = (_, y) -> y + 1i64
        , combiner      = (x, y) -> x + y
        , finisher      = x -> x
        }

    pub def toList(): Collector[a, List[a], List[a]] = 
        { supplier      = _ -> Nil
        , accumulator   = (x, xs) -> x :: xs
        , combiner      = (xs, ys) -> xs ::: ys
        , finisher      = xs -> xs
        }

    pub def toSet(): Collector[a, Set[a], Set[a]] with Order[a] = 
        { supplier      = _ -> Set.empty()
        , accumulator   = (x, s) -> Set.insert(x, s)
        , combiner      = (s1, s2) -> Set.union(s1, s2)
        , finisher      = s -> s
        }

    pub def listcollect(c: Collector[a, c, ans], l: List[a]): ans = 
        def loop(ac, xs) = match xs {
            case Nil     => let f = c.finisher; f(ac)
            case x :: rs => let f = c.accumulator; let ac1 = f(x, ac); loop(ac1, rs)
        };
        let f = c.supplier;
        loop(f(), l)
    

}

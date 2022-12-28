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
    
    type alias Collector[a: Type, acc: Type, ans: Type, ef: Bool] = 
        { supplier      = Unit -> acc \ ef
        , accumulator   = a -> acc -> acc \ ef
        , finisher      = acc -> ans \ ef
        }

}

namespace DataLib/Collector {

    use DataLib.Collector


    pub def collect(c: Collector[a, acc, ans, ef], t: t[a]): ans \ ef with Foldable[t] = 
        let st = unsafe_cast ((c.supplier)()) as _ \ ef; 
        unsafe_cast ((unsafe_cast Foldable.foldLeft(flip(c.accumulator), st, t) as _ \ ef)|> c.finisher) as _ \ ef



    

}

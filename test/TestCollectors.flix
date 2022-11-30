/*
 * Copyright 2020 Stephen Tetley
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



namespace TestCollectors {

    use DataLib/Collector.collect;

    /////////////////////////////////////////////////////////////////////////////
    // averagingInt32                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def averagingInt3201(): Bool =
        let l = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: Nil;
        let ans = collect(DataLib/Collectors.averagingInt32(identity), l);
        ans == 5.5f64


    /////////////////////////////////////////////////////////////////////////////
    // filtering                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filtering01(): Bool =
        use DataLib/Collectors.filtering;
        let l = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: Nil;
        let c1 = filtering(i -> i < 5, DataLib/Collectors.averagingInt32(identity));
        let ans = collect(c1, l);
        ans == 2.5f64

}

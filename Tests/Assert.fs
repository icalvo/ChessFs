module Assert

open Expecto

let supersetOf expected actual =
    Expect.containsAll actual expected "Not contained"

let contains item actual =
    Expect.contains actual item ""

let doesNotContain item (actual: seq<'T>) =
    Expect.throws (fun() -> contains item actual) "Contained"
module Assert
open Xunit
open System.Collections.Generic

let supersetOf (expected: seq<'T>) (actual: seq<'T>) =
    Assert.Superset<'T>(new HashSet<'T>(expected), new HashSet<'T>(actual))

let contains item (actual: seq<'T>) =
    Assert.Contains<'T>(item, new HashSet<'T>(actual))

let doesNotContain item (actual: seq<'T>) =
    Assert.DoesNotContain<'T>(item, new HashSet<'T>(actual))

﻿let f1 () = [1UL..10UL]
let f2 () = [10UL..1UL]
let f3 () = [1UL..1UL..10UL]
let f4 () = [1UL..2UL..10UL]
let f5 () = [10UL..1UL..1UL]
let f6 start = [start..10UL]
let f7 finish = [1UL..finish]
let f8 (start: uint64) finish = [start..finish]
let f9 start = [start..1UL..10UL]
let f10 step = [1UL..step..10UL]
let f11 finish = [1UL..1UL..finish]
let f12 start step = [start..step..10UL]
let f13 start finish = [start..1UL..finish]
let f14 step finish = [1UL..step..finish]
let f15 (start: uint64) step finish = [start..step..finish]
let f16 f = [f ()..10UL]
let f17 f = [1UL..f ()]
let f18 (f: unit -> uint64) g = [f ()..g()]
let f19 f = [f ()..1UL..10UL]
let f20 f = [1UL..f ()..10UL]
let f21 f = [1UL..1UL..f ()]
let f22 (f: unit -> uint64) g h = [f ()..g ()..h ()]

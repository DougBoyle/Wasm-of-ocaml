type planet = { mutable x : float; mutable y : float;  mutable z : float;
                mutable vx: float; mutable vy: float;  mutable vz: float;
                mass : float }

val advance : planet array -> int -> float -> unit
val energy : planet array -> int -> float
val offset_momentum : planet array -> int -> unit

val bodies : planet array

val f : float

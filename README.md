Partial implementation of the ExtractVector algorithm (Rump, Ogita, Oishi 2008) in OCaml.

When you sum a bunch of floating-point numbers, small values get absorbed by large ones — this is called catastrophic cancellation. ExtractVector fixes this by splitting numbers into a "high part" (exact) and a "low part" (residual error) using a clever trick.
It's also order-invariant, so you can use it in distributed systems where different nodes sum in different orders.

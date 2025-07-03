(* int -> int *)
fun cube n = n * n * n;

(* real -> real *)
fun cubeR n : real = n * n * n

(* int*int*int -> bool *)
fun triangle (a,b,c) = (a + b >= c) andalso (a + c >= b) andalso (b + c >= a);

(* real*real*real -> bool *)
fun triangleR (a : real,b : real,c : real) = (a + b >= c) andalso (a + c >= b)
andalso (b + c >=
  a);

(* int*int*int -> bool *)
fun pythagorean (a,b,c) = (a*a + b*b = c*c);

(* (int*int)*(int*int) -> (int*int) *)
fun addPairs ((a,b),(c,d)) = (a+c,b+d);

(* int -> int *)
fun abs n = if (n < 0) then ~n else n;

(* int -> int *)
fun makeEven n = if (n mod 2 = 0) then n else (n+1);

fun fib 0 = 0 | fib 1 = 1 | fib n = (fib (n-1)) + (fib (n - 2))
val x = fib 10
fun fizz n = (n mod 13 = 0)
val y = fizz 26

(* CSC372 HW3 *)
(* @author Sebastian Arana *)

(* int -> int *)
(* 
* The base case tests for int 0,
* so the parameter is an int.
* The * and + operators are performed
* on int parameters returning an int.
*)
fun sumSquares 0 = 0
  | sumSquares n = n*n + sumSquares (n-1);

(* int -> bool *)
(* 
* The base case test for int 0,
* so the parameter is an int.
* The andalse operator produces a bool,
* so this returns a boolean.
*)
fun isPower2 0 = false
  | isPower2 1 = true
  | isPower2 n = (n mod 2 = 0) andalso isPower2 (n div 2);

(* (int*int) -> bool *)
(* 
* The mod operator expects ints,
* so the parameters are ints.
* The base case returns one of the
* parameters, also an int.
*)
fun gcd (x,0) = x
  | gcd (x,y) = gcd (y, x mod y);

(* int -> real *)
(* 
* The base case compares int 1,
* so the parameter is an int.
* The return value is real because
* real division is performed.
*)
fun harmonic 1 = 1.0
  | harmonic n = 1.0/(real n) + harmonic (n-1);

(* int -> int *)
(* 
* The base case compares int 0,
* so the parameter is an int.
* The + operator produces an int
* here so it returns an int.
*)
fun padovan 0 = 1
  | padovan 1 = 1
  | padovan 2 = 1
  | padovan n = padovan (n-2) + padovan(n-3);

(* char list -> real *)
(* 
* The parameter has cons used, and str
* function on the element which expects chars.
* This is a char list, and the string is concatenated
* producing a string return value.
*)
fun ourImplode [] = ""
  | ourImplode (x::xs) = (str x) ^ ourImplode xs;

(* a' list -> 'a *)
(* 
* The cons operator expects a list,
* but the type is not specified.
* the second parameter is an int because
* it is compared int 0.
* This returns the same type as the list
* because the head is returned.
*)
fun get (x::xs,0) = x
  | get (x::xs,n) = get (xs,n-1);

(* int list -> int *)
(* 
* The cons operator detects a list,
* and the list type is int due to the mod operator.
* This returns an int by the int operator +
*)
fun sumEven [] = 0
  | sumEven (x::xs) = if (x mod 2 = 0) then (x + sumEven xs) else (sumEven xs);

(* int list -> bool *)
(* 
* The cons operator detects a list,
* and the type is int due to the mod operator.
* This returns a boolesn due to the andalso operator.
*)
fun allOdd [] = true
  | allOdd (x::xs) = (x mod 2 = 1) andalso allOdd xs;

(* a' list -> 'a *)
(* 
* The cons operator detects a list,
* but the type is not speficied by operator.
* This returns the head of the list, so same type. 
*)
fun last [] = 0 
  | last (x::[]) = x
  | last (x::xs) = last xs;

(* (a'list*int*int) -> 'a list *)
(* 
* The first param is compared to [], so it is a list.
* The second has the + operator with 1, so it is an int,
* and the third is compared to the second making it also an int.
* This returns a list through the cons operator.
*)
fun subList ([],_,_) = []
  | subList (x,lo,hi) = if (lo > hi) then [] else (get (x,lo))::subList(x,lo+1,hi);

(* int list*int -> int list *)
(* 
* The cons operator expects a list, and the type
* is revealed by the * operator which expects the
* second parameter to also be a int.
* This returns a list by the cons operator with ints
*)
fun multiplyList ([],_) = []
  | multiplyList (x::xs,n) = (x*n)::multiplyList(xs,n);

(* int list*int -> int list *)
(* 
* The cons operator expects a list, and the type
* is revealed by the > operator which defaults to int.
* This also applies to the second parameter.
* Returns an int list through the cons operator with ints
*)
fun gtList ([],_) = []
  | gtList (x::xs,n) = if (x > n) then x::gtList(xs,n) else gtList(xs,n);

(* char -> bool *)
(* 
* The parameter is a character due to being matched
* to characters. The return values are all booleans.
* 
*)
fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false;

(* a' list -> a' list *)
(* 
* The parameter is a list due to cons.
* No type is specified.
* The return value is also a list
* with two other lists being appended.
*)
fun ourRev [] = []
  | ourRev (x::xs) = (ourRev xs) @ [x];

(* a' list list -> a' list *)
(* 
* The cons operator expects a list parameter,
* and the type is revealed as list by the @ operator.
* The sublist's type is not specified.
* Returns a list of the same type through the @ operator.
*)
fun flatten [] = []
  | flatten [x] = x
  | flatten (x::xs) = x @ flatten xs;

(* 'a list*'b list -> ('a*'b) list *)
(* 
* The cons operator expects both parameters to be lists.
* Their types are not specified.
* Returns a list by the cons operator,
* and its type is tuples of the parameter types
* in parentheses.
*)
fun pair ([],[]) = []
  | pair ([],_) = []
  | pair (_,[]) = []
  | pair (x::xs,y::ys) = (x,y)::pair(xs,ys);

(* int list -> bool *)
(* 
* The cons operator expects a list parameter.
* Its type is defaulted to int through the <= operator.
* Returns a boolean explicitly.
*)
fun isAscending [] = true
  | isAscending [_] = true
  | isAscending (x::xs::xss) = if (x > xs) then false else isAscending (xs::xss);

(* int*'a list -> 'a list *)
(* 
* First parameter sees - operator, so defaults to int.
* Second parameter has cons operator, so must be list.
* Its type is not operated on, so variable type.
* Returns a list of this type through @ operator.
*)
fun cycle (_,[]) = []
  | cycle (0,x) = x
  | cycle (n,x::xs) = cycle (n-1,(xs @ [x]));

(* 'a list -> 'a list *)
(* 
* Expects list parameter through cons operator.
* Type is not operated on, so variable.
* Returns list type through @ operator.
*)
fun mirror [] = []
  | mirror (x::xs) = [x] @ mirror xs @ [x];

(* ''a list -> bool *)
(* 
* Expects list type through cons operator.
* Type sees = operator only, which is variable
* but must be equality type.
* Returns boolean explicitly.
*)
fun repeats [] = false
  | repeats [_] = false
  | repeats (x::xs::xss) = if x = xs then true else repeats (xs::xss);

(* ''a list*''a list -> bool *)
(* 
* Expects list type by cons operator.
* Type has = operator only, which is variable
* but must be equality type.
* Returns boolean explicitly.
*)
fun suffix ([],_) = true
  | suffix (_,[]) = false
  | suffix (x,y::ys) = if (x = y::ys) then true else suffix(x,ys); 

(* int list -> int list *)
(* 
* Expects list type through cons operator.
* Compares head to int 1 so type is int list.
* Returns list through cons operator.
*)
fun remove1s [] = []
  | remove1s (1::xs) = remove1s xs
  | remove1s (x::xs) = x :: remove1s xs;

(* 'a list -> bool *)
(* 
* Expects list through cons operator.
* Type is variable and does not see any operators.
* Returns boolean explicitly.
*)
fun isOddLength [] = false
  | isOddLength [_] = true
  | isOddLength (x::xs::xss) = isOddLength xss;

(* (int*int) list -> int*int *)
(* 
* Expecs list type through cons operator,
* and type is unpacked in parentheses.
* These are operated with the return type
* using +, so they are ints.
* Returns tuple in parentheses with int output of +.
*)
fun sumPairs [] = (0,0)
  | sumPairs ((a,b)::xs) =
    let
      val (sumA,sumB) = sumPairs xs
    in
      (sumA + a, sumB + b)
    end;

(* int list -> (int list*int list) *)
(* 
* Cons operator expects list parameter.
* List type is ints by the mod operator.
* Returns a tuple in parentheses, and the types are
* lists with the cons operator. The types of lists
* are both ints after seeing the mod operator.
*)
fun evenOdds [] = ([],[])
  | evenOdds (x::xs) =
    let
      val (even,odd) = evenOdds xs
    in
      if (x mod 2 = 0) then (x::even,odd) else (even,x::odd)
    end;

(* int list * int -> int *)
(* 
* Expects a list parameter with cons, and its
* type as well as the second parameter are ints
* by default when operated on using +.
* Returns an int in base case which is the same
* as the second parameter.
*)
fun addAll ([],n) = n
  | addAll (x::xs,n) = addAll(xs,(n + x));

(* ('a -> bool)*'a list -> 'a list*'a list *)
(* 
* Parameter 1: expects function as it is called on something.
* This function can satisfy a conditional, so it must return a bool.
* Its input type is variable as it is onle extracted from a list.
* Parameter 2: This should be a list by the cons operator. The type is
* variable, but it is the same input type as the function because the function
* is called on elements of the list.
* Returns a tuple in parentheses, and the types are both lists by cons.
* The types of both lists are the same variable as the parameter list
* having only been extracted and sorted with no operator applied.
*)
fun partition (_,[]) = ([],[])
  | partition (func,x::xs) =
    let
      val (trues,falses) = partition (func,xs)
    in
      if (func x) then (x::trues,falses) else (trues,x::falses)
    end;

(* 'a list * 'b * ('a * 'b -> 'b) -> 'b *)
(* 
* Parameter 1: Expects list type with cons operator.
* Type of the list is variable as the elements do not operate.
* Parameter 2: Not operated on except in parameter 3's function,
* must be a variable type.
* Parameter 3: Is called on two other variables, so must be a function.
* Its input type is a tuple of Param 1's list type and Param 2's type.
* Its output is the same type as Param 2 because it is inputted as Param 2
* recursively.
* Returns Param 2 explicitly in the base case, so must be Param 2's type.
*)
fun collapse ([],n,_) = n
  | collapse (x::xs,n,func) = collapse (xs, (func (x,n)), func);


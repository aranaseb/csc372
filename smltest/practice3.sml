fun applyToAllT (_,[]) = []
| applyToAllT (f, x::xs) = (f x)::applyToAllT(f,xs);

fun combineListsT (_,[],_) = []
| combineListsT (_,_,[]) = []
| combineListsT (f,x::xs,y::ys) = (f(x,y))::combineListsT(f,xs,ys);

fun applyToAll _ [] = []
| applyToAll f (x::xs) = (f x)::(applyToAll f xs);

fun combineLists _ [] _ = []
| combineLists _ _ [] = []
| combineLists f (x::xs) (y::ys) = f(x,y)::(combineLists f xs ys);

val incrementAll = applyToAll (fn x => x + 1);
val isEven = applyToAll (fn x => x mod 2 = 0);
val toReal = applyToAll real;
val dropFirstChar = applyToAll (fn s => implode(tl(explode s)));

val addLists = combineLists (op +);
val multRealLists = combineLists (fn (a:real,b) => a*b);
val concatStrings = combineLists (op ^);
val addIntRealLists = combineLists (fn (i,r) => real i + r);

val squareList = map (fn x => x*x);
val evens = foldr (fn (e,l) => if e mod 2 = 0 then e::l else l) [];
val revListPos = foldl (fn (e,l) => if e > 0 then e::l else l) [];





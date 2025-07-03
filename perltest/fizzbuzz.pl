for $i (1..100) {
	print "Fizz" if $i % 7 == 0;
	print "Buzz" if $i % 13 == 0;
	print "$i" if ($i % 7 != 0 and $i % 13 != 0);
	print " ";
	print "\n" if $i % 10 == 0;
}

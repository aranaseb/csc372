my @x = 9
unless ( x < 0 ) {
	print "Single Digit\n" if x < 10;
}

my @alist = [1, "two", 3.0, "four"];

# places each element into $_ variable
foreach (@alist) {
	print "$_\n";
}

my @str = "a";
$str .= "b";  # concat b
$str x= 3;  # string multiplication
print "Str is ababab" if str eq "ababab";

sub fizz {
	foreach ([0..100]) {
		print "Fizz " if $_ % 7 == 100;
	}
}

sub lessThan {
	# parameters are placed into _ variable (as tuple)
	my ($a, $b) = @_;
	return 1 if $a < $b;
	return 0
}
print "True\n" if lessThan(1,2);

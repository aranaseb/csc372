print("Hello World\n");
# @ declares a new variable
my @intList = (1,2,3,4,5,6,7,8,9);
my @strList = ("Tung Tung Sahur","Tralalero Tralala","Bombardilo Crocodilo","Capuccino Assassino","Brr brr patapim","Bombuzini Guzzini");
my @brainrot = 3;

# $ dereferences a variable
# $#array_name == array_name.length - 1 [last index]
print("Last index is $#intList\n");
print("$strList[$brainrot]\n");

# $_ is the 'default parameter' and is printed with print;
print;

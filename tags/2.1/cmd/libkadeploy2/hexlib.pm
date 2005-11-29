package libkadeploy2::hexlib;

sub hexalize($) 
{
    my $number = shift;
    if ($number<16) {
	return (sprintf "0%X", $number);
    } else {
	return (sprintf "%X", $number);
    }
}

sub unhexalize($)
{
    my $number = shift;
    return hex($number);
}

1;

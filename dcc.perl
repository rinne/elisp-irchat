#!/usr/bin/perl
#!/usr/local/bin/perl
#!/usr/bin/perl -w
# -*- perl -*-
# $Id: dcc.perl,v 1.1 1997/02/21 14:51:43 too Exp $
#
# see file irchat-copyright.el for change log and copyright info
#
# Created: Mon May  8 12:33:04 1995 too
# Last modified: Fri Feb 21 16:41:41 1997 too
#

$AF_INET = 2;
$SOCK_STREAM = 1;

$SOL_SOCKET = 0xffff;

$SO_REUSEADDR = 0x04;
$SO_KEEPALIVE = 0x08;

#$sockaddr = 'S n a4 x8';
$sockaddr = 'S n N x8';	# Notice `N' does the trick.


&usage unless $action = shift(@ARGV);


if ($action eq 'receive')
{
    die "Wrong number of parameters\n",
    "Usage: dcc receive <host> <port> <size> <filename>\n"
	unless (($host = shift(@ARGV)) && ($port = shift(@ARGV)) && 
		($size = shift(@ARGV)) && ($filename = shift(@ARGV)));

    socket(S, $AF_INET, $SOCK_STREAM, 0) || die "ERROR socket: $!";
    setsockopt(S, $SOL_SOCKET, $SO_KEEPALIVE, 1);

    $that = pack($sockaddr, $AF_INET, $port, $host);
    connect(S, $that) || die "ERROR connect: $!";

    open(OUTPUT, ">$filename") || 
	die "ERROR Cannot open output file `", $filename, "'\n";

    $toread = ($size > 32768)? 32768: $size;

    $shwprg = $size / 5;
    $bytesreceived = 0;

    while ($bytesreceived < $size && ($len = sysread(S, $buf, $toread)) > 0)
    {

	print $len, "\n";
	$bytesreceived+= $len;
	&writeall(OUTPUT, $buf, $len);
	&writeall(S, pack("N", $bytesreceived), 4);

	if ($bytesreceived > $shwprg)
	{
	    printf("DCC %s %d%% (%d/%d bytes) received\n", $filename,
		   100 * $bytesreceived / $size, $bytesreceived, $size);
	    $shwprg += $size / 5;
	}
    }

    printf(STDERR "ERROR %s: %d bytes lost\n", 
	   $filename, $size - bytesreceived) if ($bytesreceived != $size);

    printf("*** DCC file %s (%d bytes) received\n", $filename, $bytesreceived);
    close OUTPUT;
    close S;
    exit 0;
}

if ($action eq 'send')
{
    die "Wrong number of parameters\n",
    "Usage: dcc send <port> <filename>\n"
	unless (($port = shift(@ARGV)) && ($filename = shift(@ARGV)));

    open(INPUT, "$filename") 
	|| die "ERROR Can not open file `", $filename, "'\n";

    chop($host = `hostname`);
    ($name, $aliases, $type, $len, $thisaddr) = gethostbyname($host);
    
    exit 2 unless $thisaddr;

    socket(SS, $AF_INET, $SOCK_STREAM, 0) || die "ERROR socket: $!";
    setsockopt(SS, $SOL_SOCKET, $SO_REUSEADDR, 1);

    $this = pack($sockaddr, $AF_INET, $port, 0);
    for ($tries = 0; $tries < 10; $tries++)
    {
	bind(SS, $this) && last;

	die "bind $!" if ($tries == 9);

	print STDERR "Binding stream socket ($!) retry in 20 seconds.\n";
	sleep(20);
    }

    listen(SS, 5) || die "ERROR listen: $!";

    $size = -s $filename;
    printf("DCC send %s %d %u %d\n",
	   $filename, $port, unpack("N", $thisaddr), $size);

    accept(S, SS) || die "ERROR accept $!";
    close SS;

    READ: while ($len = sysread(INPUT, $buf, 32768))
    {
	&writeall(S, $buf, $len);
	$bytessent += $len;
#	print $bytessent, " ", $len, "\n";
	do {
	    $ln2 = sysread(S, $buf, 4); # inconsistent w/ respect to `writeall'
	    last READ unless $ln2;
	} while (unpack("N", $buf) != $bytessent);
    }
    unless ($ln2) {
	print STDERR "ERROR ", $filename, " not successfully sent\n";
    }
    else {
	print "*** DCC file ", $filename, " sent\n";
    }
    close INPUT;
    close S;
    exit 0;
}

if ($action eq 'resolve') 
{
    die "Wrong number of parameters\n",
    "Usage: dcc resolve <host\n" unless ($host = shift(@ARGV));
	
    do
    {
	($name, $aliases, $type, $len, $thisaddr) = gethostbyname($host);
	# perhaps this is sufficient, otherwise make `inet_addr' using pack()
	
	if ($thisaddr)
	{
	    print unpack("N", $thisaddr), "\n";
#	    print unpack ("L", pack("N", $thisaddr))); # ntohl
	}
	else
	{
	    print "0\n";
	}

    } while ($host = shift(@ARGV));

    exit 0;
}

&usage;

sub usage
{
die "Usage: dcc send <port> <filename>\n",
    "Usage: dcc receive <host> <port> <size> <filename>\n",
    "Usage: dcc listen <port>\n",
    "Usage: dcc resolve <host> [<host>]\n";
}

sub writeall
{
    local($fd, $buf, $len) = @_;
    $offset = 0;

    while ($len)
    {
#	print $len, "\n";
	$written = syswrite($fd, $buf, $len, $offset);
	die "ERROR syswrite: $!" unless defined $written;
	    
	$len-= $written;
	$offset+= $written;
    }
}


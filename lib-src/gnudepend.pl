#!/usr/bin/perl

while (<>)
{
    if (/\\$/)
    {
        chop;
        $foo .= $_;
    }
    else
    {
        @foo = split (/[ \\:]+/, $foo);
        $filename = $foo[0];
	if (($filename =~ /^unex/) ||
	    ($filename =~ /^sgiplay/) ||
	    ($filename =~ /^Extern/) ||
	    ($filename =~ /^extw/))
	{
	    $foo = "";
	    next;
	}
        @foo = grep (!/\.c$/, @foo);
        @foo = grep ((s/\/.*lwlib\//\$(LWLIBSRCDIR)\//, 1), @foo);
        @foo = grep (!/lisp\.h/, @foo);
	@foo = grep (!/lisp\.h/, @foo);
	@foo = grep (!/lisp-union\.h/, @foo);
	@foo = grep (!/lisp-disunion\.h/, @foo);
	@foo = grep (!/lrecord\.h/, @foo);
	@foo = grep (!/emacsfns\.h/, @foo);
	@foo = grep (!/symeval\.h/, @foo);
	@foo = grep (!/symsinit\.h/, @foo);
	@foo = grep (!/syssignal\.h/, @foo);
	@foo = grep (!/intl\.h/, @foo);
	@foo = grep (!/tt_c\.h/, @foo);
	@foo = grep (!/descrip\.h/, @foo);
        shift @foo;
        foreach $i (0 .. $#foo)
	{
            $foo[$i] = $filename . ": " . $foo[$i];
	}
	print $filename . ": config.h\n";
        print join ("\n", @foo);
        print "\n";
        $foo = "";
    }
}

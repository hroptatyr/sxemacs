#! bash

yorn()
{
	echo -n " [y/n] "
	read YN junk
	if [ "$YN" = "n" ]
	then
		return -1;
	else
		return 0;
	fi
}

echo -n "checking for cygwin..."

if ! uname -v
then
    echo "couldn't find uname please add cygwin to your path."
    exit -1
fi

OSversion="`uname -v | sed 's/^\(.\).*$/\1/'`"

shell=`type sh | sed 's/sh is //'`
distdir=`dirname $shell | sed 's!^//\(.\)/\(.*\)!\1:/\2!'`

echo "cygwin installed in $distdir"

echo "checking paths ..."

if [ ! -d "/bin" ]; then
    echo "You don't have a /bin directory.  Would you like to mount cygwin as /bin ?"
    if yorn; then
	mkdir /bin
	mount -b $distdir /bin
    fi
elif [ "$distdir" != "/bin" ]; then
    echo "Warning: you have /bin but it's not the cygwin installation."
fi

if [ ! -d "/tmp" ]; then
    echo -n "You don't have /tmp - create it?"
    if yorn; then
	mkdir /tmp
    fi
else
    echo "you have /tmp"
fi

if [ ! -d "/etc" ]; then
    echo -n "You don't have /etc - create it?"
    if yorn; then
	mkdir /etc
    fi
else
    echo "you have /etc"
fi

if [ -d "/etc" ]
then
    if [ ! -f "/etc/termcap" ]; then
	echo -n "You don't have /etc/termcap - create it?"
	if yorn; then
	    if [ ! -f "$distdir/../etc/termcap" ]
	    then
		distdir=`mount | grep "$distdir" | sed -e "s/ .*$//"`
		echo "Retrieving termcap from $distdir/../etc"
	    fi
	    if [ -f "$distdir/../etc/termcap" ]
	    then 
		cp "$distdir/../etc/termcap" /etc
	    else
		echo "Error: can't find termcap file"
	    fi
	fi
    else
	echo "you have /etc/termcap"
    fi

    if [ ! -f "/etc/passwd" ]; then
	echo -n "You don't have /etc/passwd - create it?"
	if yorn; then
	    if [ "$OS" = "Windows_NT" ]
	    then
		echo -n "Running on NT, create domain or local password file [d/l] "
		read DL junk
		if [ "$DL" = "d" ]
		then
		    mkpasswd -d > /etc/passwd
		else
		    mkpasswd -l > /etc/passwd
		fi
	    else
		echo "Please enter your userid e.g. andyp"
		read userid junk
		echo "Please enter your user name e.g. Andy Piper"
		read username junk
		echo "Administrator::500:513:::/bin/sh" > /etc/passwd
		echo "$userid::1000:513:$username::/bin/sh" >> /etc/passwd
	    fi
	fi
    else
	echo "you have /etc/passwd"
	userid=`id | sed -e "s/[^(]*(\([^)]*\)).*/\1/"`
    fi

    echo "userid is $userid"

    if [ ! -f "/etc/group" ]; then
	echo -n "You don't have /etc/group - create it?"
	if yorn; then
	    if [ "$OS" = "Windows_NT" ]
	    then
		echo -n "Running on NT, create domain or local group file [d/l] "
		read DL junk
		if [ "$DL" = "d" ]
		then
		    mkgroup -d > /etc/group
		else
		    mkgroup -l > /etc/group
		fi
	    else
		echo "None::513:" > /etc/group
		echo "Everyone::0:" >> /etc/group
	    fi
	fi
    else
	echo "you have /etc/group"
    fi

    if [ ! -f "/etc/hosts" ]; then
	echo -n "You don't have /etc/hosts - create it?"
	if yorn; then
	    mname=`uname -n`
	    echo "Machine name is $mname"
	    echo -n "Please enter your ip address "
	    read mipaddr junk
	    echo "$mname $mipaddr" > /etc/hosts
	    echo "localhost 127.0.0.1" >> /etc/hosts
	fi
    else
	echo "you have /etc/hosts"
    fi
else
    echo "Can't create /etc files because /etc does not exist"
fi

echo "checking environment ..."

if [ "$HOME" = "" ]; then
    echo -n "HOME is not set, rectify?"
    if yorn; then
	if [ "$OS" = "Windows_NT" ]
	then
	    echo "please enter your home path [/winnt/profiles/$userid]"
	    read HOME junk
	    if [ "$HOME" = "" ]; then
		HOME="/winnt/profiles/$userid"
	    fi
	else
	    echo "please enter your home path [/]"
	    read HOME junk
	    if [ "$HOME" = "" ]; then
		HOME="/"
	    fi
	fi

	echo "HOME=$HOME; export HOME" >> $HOME/.bashrc
    fi
else 
    echo "HOME is $HOME"
fi

if [ "$TERM" != "ansi" -a "$TERM" != "linux" ]; then
    echo -n "TERM is not set to linux or ansi, rectify?"
    if yorn; then
	echo "TERM=linux; export TERM" >> $HOME/.bashrc
    fi
else 
    echo "TERM is $TERM"
fi

if echo $CYGWIN32 | grep -w tty; then
    echo "CYGWIN32 is $CYGWIN32"
else 
    echo "CYGWIN32 does not contain \"tty\" terminal may be deficient"
fi

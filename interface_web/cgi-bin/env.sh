#!/bin/sh

echo Content-Type: text/html
echo ""

/bin/cat << EOM
<HTML>
<HEAD><TITLE>File Output: ls </TITLE>
</HEAD>
<BODY bgcolor="#cccccc" text="#000000">
<HR SIZE=5>
<H1>File Output: ls </H1>
<HR SIZE=5>
<P>
<SMALL>
<PRE>
EOM

env

/bin/cat << EOM
</PRE>
</SMALL>
<P>
</BODY>
</HTML>
EOM

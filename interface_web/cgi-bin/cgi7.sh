#!/bin/sh

echo Content-Type: text/html
echo ""

# our html header
echo "<html>"
echo "<head><title>Hello CGI</title></head>"
echo "<body>"


# test if any parameters were passed
if [ $CMD ]
then
  case "$CMD" in
    ifconfig)
      echo "Output of ifconfig :<pre>"
      /sbin/ifconfig
      echo "</pre>"
      ;;

    uname)
      echo "Output of uname -a :<pre>"
      /bin/uname -a
      echo "</pre>"
      ;;

    dmesg)
      echo "Output of dmesg :<pre>"
      /bin/dmesg
      echo "</pre>"
      ;;

    ls)
      echo "Output of ls $FOLDER :<pre>"
      /bin/ls "$FOLDER"
      echo "</pre>"
      ;;

    *)
      echo "Unknown command $CMD<br>"
      ;;
  esac
fi

# print out the form
echo "Choose which command you want to run"
echo "<form method=get>"
echo "<input type=radio name=cmd value=ifconfig checked> ifconfig <br>"
echo "<input type=radio name=cmd value=uname> uname -a <br>"
echo "<input type=radio name=cmd value=dmesg> dmesg <br>"
echo "<input type=radio name=cmd value=ls> ls  -- folder <input type=text name=folder value=/mnt/flash><br>"
echo "<input type=submit>"
echo "</form>"
echo "</body

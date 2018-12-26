#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
  eval `dbus-launch --sh-syntax`
  echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
else
  echo "Session started with $DBUS_SESSION_BUS_ADDRESS"
fi

~/bin/init &
start-pulseaudio-x11 &
# exec ck-launch-session dbus-launch --exit-with-session dwm
# exec ck-launch-session dbus-launch --exit-with-session windowchef -c ~/.config/windowchef/windowchefrc
exec ck-launch-session dbus-launch --exit-with-session bspwm

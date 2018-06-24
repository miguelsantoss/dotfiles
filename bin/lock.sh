TMPBG=/tmp/screen.png

resolution=$(xdpyinfo | grep dimensions | awk '{print $2}')
ffmpeg -y -loglevel 0 -s "$resolution" -f x11grab -i $DISPLAY -vframes 1 -vf "gblur=sigma=8" $TMPBG
i3lock -i $TMPBG
rm $TMPBG

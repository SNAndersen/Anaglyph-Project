#!/bin/bash
trap "exit" 2   # exit with ctrl c
# q and esc key stops current playing video and loops to the next  - ctrl c stops execution of the entire script
    
PLAYLIST="playlist.pls"

# Play the playlist if it exists
if [ -e "$PLAYLIST" ]
then
    IFS=$'\012'	# IFS: Internal field separator ; \12=form feed
    for file in $(cat "$PLAYLIST")
    do
	IFS=$' \t\n'
                -e  # exit from failure do not ignore SIGINT
                omxplayer $file  # works ONLY with file path and file names without space
                #quotes embedded in $file won't work so -b --anaglyph n --win 0,0,1920,1080 "file name and path" can not be parsed 

                #quotes around $file makes it possible to have spaces in path and file name
                #omxplayer -b --anaglyph 1 --win 0,0,1920,1080 "$file"  # hard coded for red/cyan glasses and sbs video
        done
fi
# Anaglyph-Project
Anaglyph player for Raspberry Pi


Installation:

All executables and scripts are located in folder Anaglyph.
 
Place folder Anaglyph in /home/pi if possible, then you won't need to make any changes in the scripts.

Copy Anaglyph Playlist from folder Anaglyph to desktop.

Use Main Menu Editor to place Anaglyph Player and  Anaglyph Clipboard in Sound and Video, or
copy Anaglyph Player and Anaglyph Clipboard to desktop.

Source code for all executables is found in folders:
Anaglyph_Player and Anaglyph_Clp
Just open the .lpr ( project file ) with Lazarus.

Use:
The scope of this project has been to demonstrate that anaglyph video's can be played on a Raspberry Pi.
To do so two wrappers for omxplayer has been made with Lazarus Pascal Compiler.

Wrapper one:
Anaglyph Player:
Plays the selected file in anaglyph mode if the file is stereoscopic.

First select your glasses ( Red / Cyan or Green / Magenta ), then open a stereoscopic file.

The software reads the file header if it is a MKV file. If the header contains stereoscopic information, that information will be used to set ( HSBS or HOU ).

If the filename contains following ( SBS, HSBS, TAB, HTAB ) that information will be used to select the stereo mode.

Eksample:
myfile.3D.SBS.mp4		3D side by side
myfile.3D.TAB.mkv		3D top and bottom

Spaces are allowed if file name and file path.

If no information exists, the file will be played as normal ( Not anaglyph ).

Subtitles:
If you want subtitles, place a subtitle file (srt) with the same name as the video file.

If there is no depth in the video, try to restart it with Swap left/right enabled.

Your selection of anaglyph glasses and Swap left/right will be saved in an inifile.

Press Full Screen for full screen view, movie will restart.

Use keyboard commands to control omxplayer, commands can be found here:
https://www.raspberrypi.org/documentation/raspbian/applications/omxplayer.md

All keyboard commands except arrow keys are supported.

If movie search  is desired ( arrow keys ), you can press Copy to Clipboard and open LXTerminal.
Place the curson in LXTerminal, right click mouse and select insert, press return and the movie plays.


Wrapper two:
Anaglyph Clipboard:

Works as described above but shows no video, it minimizes after stereo mode is detected and places the file information for LXTerminal on the clipboard.

You can create a playlist of video files from a folder.
If needed you can randomize the playlist, it is done directly in the playlist file.

Now to play the playlist two scripts has been made.

One:
A desktop script ( Anaglyph Playlist ) that can start a bash script that plays the entire playlist .

Two:
A bash script ( read_playlist.sh ) that loops through the entire playlist.
Keypress q or esc stops execution and jumps to next movie, ctrl c stops the entire loop.



Have fun.

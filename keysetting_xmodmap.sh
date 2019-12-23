#!/bin/sh
xmodmap -e 'remove Lock = Caps_Lock'
xmodmap -e 'keysym Escape = Caps_Lock'
xmodmap -e 'keysym Caps_Lock = Escape'
xmodmap -e 'add Lock = Caps_Lock'
xmodmap -e 'remove shift = Shift_R'
xmodmap -e 'keycode 62 = backslash bar backslash bar'
xmodmap -e 'remove mod1 = Alt_R'
xmodmap -e 'keycode 108 = Hangul'
xmodmap -e 'remove control = Control_R'
xmodmap -e 'keycode 105 = Hangul_Hanja'
xmodmap -pke > ~/.Xmodmap

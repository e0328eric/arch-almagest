#!/bin/sh
xmodmap -e 'remove mod1 = Hangul'
xmodmap -e 'keycode 108 = Hangul'
xmodmap -e 'remove control = Hangul_Hanja'
xmodmap -e 'keycode 105 = Hangul_Hanja'
xmodmap -pke > ~/.Xmodmap

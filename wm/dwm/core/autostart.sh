#! /bin/bash

picom -b --config $HOME/.config/compton/compton.conf &
syndaemon -i 1.0 -t -K -R &
light-locker &
battery-low &
nextcloud &
nitrogen --restore &
kdeconnect-indicator &
dwmbar &

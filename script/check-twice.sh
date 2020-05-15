#! /bin/sh
# A dmenu binary check-twice script.
# Gives a dmenu prompt labeled with $1 to perform command $2
# Example:
# './check-twice "Do you want to shutdown?" "shutdown -h now"

[ $(echo -e "No\nYes" | dmenu -i -p "$1") == "Yes" ] && $2

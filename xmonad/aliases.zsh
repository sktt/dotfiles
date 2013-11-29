alias volup='amixer -q set Master 10%+'
alias voldown='amixer -q set Master 10%-'
alias getvolume="amixer sget Master | sed -n '/ft:/{;s/.* \[\([0-9]\+%\).*\]/\1/;p;}'"

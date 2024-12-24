setxkbmap dvorak
xmodmap /home/tom/Config/dotfiles/xmodmaps/caps

xinput --set-prop "CUST0001:00 06CB:76B1 Touchpad" "libinput Natural Scrolling Enabled" 1
xinput --set-prop "CUST0001:00 06CB:76B1 Touchpad" "libinput Tapping Enabled" 1
# New vertical mouse
xinput --set-prop "USB Optical Mouse" "libinput Natural Scrolling Enabled" 1
xinput --set-prop "USB Optical Mouse" "Coordinate Transformation Matrix" 1 0 0 0 1 0 0 0 1.7
# Old mouse:
# xinput --set-prop "Logitech USB Optical Mouse" "libinput Natural Scrolling Enabled" 1
# xinput --set-prop "Logitech USB Optical Mouse" "Coordinate Transformation Matrix" 1 0 0 0 1 0 0 0 1.7

FNMODE=/sys/module/hid_apple/parameters/fnmode

# https://wiki.archlinux.org/title/Apple_Keyboard#Function_keys_do_not_work
if [ -e /sys/module/hid_apple/parameters/fnmode ]; then
    if [ "$(cat $FNMODE)" != 2 ]; then
	sudo sh -c "echo 2 > /sys/module/hid_apple/parameters/fnmode"
    fi
fi

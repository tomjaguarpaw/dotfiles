setxkbmap dvorak
xmodmap /home/tom/Config/dotfiles/xmodmaps/caps

xinput --set-prop "CUST0001:00 06CB:76B1 Touchpad" "libinput Natural Scrolling Enabled" 1
xinput --set-prop "CUST0001:00 06CB:76B1 Touchpad" "libinput Tapping Enabled" 1
xinput --set-prop "Logitech USB Optical Mouse" "libinput Natural Scrolling Enabled" 1
xinput --set-prop "Logitech USB Optical Mouse" "Coordinate Transformation Matrix" 1 0 0 0 1 0 0 0 1.7

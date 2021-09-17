sudo --validate
sh ~/Config/dotfiles/xrandr/internal_monitor
sleep 1
sudo sh -c 'echo deep > /sys/power/mem_sleep && echo mem > /sys/power/state'

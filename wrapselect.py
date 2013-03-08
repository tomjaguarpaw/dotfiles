import os, sys

wrap_dir = '/home/tom/Config/Desktop/wrap'

d = {'todo': 'exec sh %%s/rxvt -cr red -e sh %s/todo' % wrap_dir,
     # ^^ This is a fairly shit way of doing substitution
     'fetchmail': 'sh %s/fetchmail',
     'mutt': 'EDITOR=jmacs xterm -bg black -fg grey -geometry 104x24 -e %s/mutt',
     'rxvt': 'exec sh %s/rxvt',
     'rxvt-screen': 'exec sh %s/rxvt-screen',
     'rxvt-tmux': 'exec sh %s/rxvt-tmux'
     }

for command in sys.argv[1:]:
	os.system(d[command] % wrap_dir)

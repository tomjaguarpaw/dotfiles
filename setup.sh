set -e

DOTFILES=$1

if [ ! -r "$DOTFILES" ]; then
    echo I could not read the directory "$DOTFILES"
    echo
    echo Usage: sh setup.sh '<path to dotfiles directory>'
    echo
    echo NB You probably want to specify an absolute path!
    exit
fi

ln -s $DOTFILES/terminfo ~/.terminfo
ln -s $DOTFILES/screenrc ~/.screenrc
ln -s $DOTFILES/tmux.conf ~/.tmux.conf
ln -s $DOTFILES/zsh/zshrc ~/.zshrc
ln -s $DOTFILES/Emacs/emacs ~/.emacs

set -e

DOTFILES=$1

if [ ! -r "$DOTFILES" ]; then
    echo I could not read the directory "$DOTFILES"
    echo
    echo Usage: sh setup.sh '<path to dotfiles directory>'
    exit
fi

ln -s $DOTFILES/terminfo ~/.terminfo
ln -s $DOTFILES/screenrc ~/.screenrc
ln -s $DOTFILES/tmux.conf ~/.tmux.conf
ln -s $DOTFILES/zsh/zshrc ~/.zshrc

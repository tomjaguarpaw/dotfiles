set -e

DOTFILES=$1

ln -s $DOTFILES/terminfo ~/.terminfo
ln -s $DOTFILES/screenrc ~/.screenrc
ln -s $DOTFILES/tmux.conf ~/.tmux.conf
ln -s $DOTFILES/zsh/zshrc ~/.zshrc

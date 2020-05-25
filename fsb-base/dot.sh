#!/usr/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
unlink ~/.ssh
unlink ~/.bashrc
unlink ~/.local/bin
unlink ~/.tmux.conf
unlink ~/.emacs.d
ln -s $DIR/dotfiles/ssh ~/.ssh
ln -s $DIR/dotfiles/bash/bashrc ~/.bashrc
ln -s $DIR/local-bin ~/.local/bin
ln -s $DIR/dotfiles/tmux/tmux.conf ~/.tmux.conf
ln -s $DIR/dotfiles/emacs ~/.emacs.d/

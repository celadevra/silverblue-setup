#!/usr/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ln -s $DIR/dotfiles/ssh ~/.ssh
ln -s $DIR/dotfiles/bash/bashrc ~/.bashrc
ln -s $DIR/local-bin ~/.local/bin

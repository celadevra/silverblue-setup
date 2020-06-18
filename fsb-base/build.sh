#!/usr/bin/bash
toolbox rm base
toolbox create -c base
toolbox run -c base sudo dnf install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
toolbox run -c base sudo dnf update -y
toolbox run -c base sudo dnf install -y git tmux neovim python3-neovim pass aria2 mpv pandoc gcc make rclone git-annex adobe-source-han-mono-fonts adobe-source-han-sans-cn-fonts adobe-source-han-serif-cn-fonts aspell tig ripgrep aspell-en emacs-nox glibc-langpack-zh langpacks-core-zh_CN emacs
# set up for blog
# toolbox run -c base sudo dnf install -y texinfo m4

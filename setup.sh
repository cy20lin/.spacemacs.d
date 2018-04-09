#!/bin/sh

pacman -Syu --needed --noconfirm

pacman -S emacs --needed --noconfirm
pacman -S mingw-w64-x86_64-emacs --needed --noconfirm
pacman -S git --needed --noconfirm

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/
pushd ~/.emacs.d/
# git checkout develop
git checkout 0fa3658cd8e283825dcd0a54ce1579dec55eb568
popd

# emacs --insecure

# javascript setup
pacman -S mingw-w64-x86_64-nodejs --needed


# c++

# treemacs setup

# python
pacman -S --needed --noconfirm \
       mingw-w64-x86_64-python3 \
       mingw-w64-x86_64-python3-pip

pip3 install python-language-server

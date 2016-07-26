#!/usr/bin/env bash
rm -rf ~/.emacs.d
ln -s `pwd`/_emacs.d ~/.emacs.d
cp _emacs.example ~/.emacs
./install_packages.sh

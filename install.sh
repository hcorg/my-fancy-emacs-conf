#!/usr/bin/env bash
ln -s `pwd`/_emacs.d ~/.emacs.d
cp _emacs.example ~/.emacs
./install_packages.sh

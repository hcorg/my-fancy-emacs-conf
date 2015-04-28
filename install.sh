#!/usr/bin/env bash
./install_packages.sh
ln -s `pwd`/_emacs.d ~/.emacs.d
cp _emacs.example ~/.emacs

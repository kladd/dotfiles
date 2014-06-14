#!/usr/bin/env bash

DOTFILES_DIR=${HOME}/.dotfiles.d
INSTALL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Copy files
for file in $(ls|grep -v "install.*\|README.*"); do
    cp -R "$PWD/$file" "$HOME/.$file"
done

# Write dotfiles info
echo "INSTALL_DIR=$INSTALL_DIR" > ${DOTFILES_DIR}/.dotfiles

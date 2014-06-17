#!/usr/bin/env bash

DOTFILES_DIR=${HOME}/.dotfiles.d
INSTALL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

git submodule update --recursive --init

mkdir out

for file in $(ls|grep -v "install.*\|README.*\|out"); do
    cp -r "$PWD/$file" "out/.$file"
done

# Copy files
pushd out
rsync --exclude ".git/" \
      --exclude ".DS_Store" \
      --exclude "install.sh" \
      --exclude "README.md" \
      -avh --no-perms . ~
popd
rm -r out

source ~/.bash_profile

# Write dotfiles info
echo "INSTALL_DIR=$INSTALL_DIR" > ${DOTFILES_DIR}/.dotfiles

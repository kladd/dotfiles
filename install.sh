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
rsync --exclude ".git" \
      --exclude ".DS_Store" \
      --exclude "install.sh" \
      --exclude "README.md" \
      -avh --no-perms . ~
popd
rm -r out

# Set up Vundle
pushd ${HOME}/.vim/bundle/Vundle.vim
if [ -f '.git' ]; then
	rm -rf .git
fi
git init
git remote add origin 'https://github.com/gmarik/Vundle.vim.git'
popd

source ~/.bash_profile

# Write dotfiles info
echo "INSTALL_DIR=$INSTALL_DIR" > ${DOTFILES_DIR}/.dotfiles

#
# git.plugin.sh
#

if which git > /dev/null; then
    alias g='git'

    alias gl='git log --decorate --abbrev-commit'
    alias gg='git log --graph --oneline --decorate'
    alias gco='git checkout'

    alias gc='git commit'
    alias gca='git commit -a'
    alias gf='git fetch'
    alias grm='git diff --diff-filter=D --name-only -z | xargs -0 git rm' 

    __git_complete gco _git_checkout
    __git_complete g __git_main

    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
fi

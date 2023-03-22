# I can't write this comment again. You know what this does.
[[ $- != *i* ]] && return

[[ -f /etc/bashrc ]] && . /etc/bashrc

shopt -s checkwinsize;
shopt -s histappend;

export HISTSIZE=5000
export HISTCONTROL=ignoredups
export HISTIGNORE="clear:bg:fg:cd:cd -:cd ..:exit:date:w:* --help:ls"

# git
if which git > /dev/null; then
    alias g='git'
    alias gc='git commit -v'
    alias gco='git checkout'

    __git_complete g __git_main
    __git_complete gco _git_checkout

    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true

    [[ -f /usr/share/git/completion/git-prompt.sh ]] && \
        . /usr/share/git/completion/git-prompt.sh
    [[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]] && \
        . /usr/share/git-core/contrib/completion/git-prompt.sh

    PS1="[\[\e[01;32m\]\u@\h\[\e[00m\] \[\e[01;34m\]\W\[\e[00m\]\[\e[0;33m\]"'$(__git_ps1 " %s")'"\[\e[0m\]]\$ "
else
    PS1="[\[\e[01;32m\]\u@\h\[\e[00m\] \[\e[01;34m\]\W\[\e[00m\]\[\e[0;33m\]\[\e[0m\]]\$ "
fi

complete -cf sudo
alias sudo='sudo -E'
alias ls='ls --color=auto'
alias ssh='TERM=xterm-256color ssh'

function emacs()
{
    (nohup emacsclient -c "$@" > /dev/null 2>&1 &) > /dev/null
}

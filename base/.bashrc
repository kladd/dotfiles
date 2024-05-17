# I can't write this comment again. You know what this does.
[[ $- != *i* ]] && return

[[ -f /etc/bashrc ]] && . /etc/bashrc

shopt -s checkwinsize;
shopt -s histappend;

export HISTSIZE=9000
export HISTCONTROL=ignoredups
export HISTIGNORE="clear:bg:fg:cd:cd -:cd ..:exit:date:w:ls"

# git
if which git > /dev/null; then
    alias g='git'
    alias gc='git commit -v'
    alias gl='git log --decorate --abbrev-commit'
    alias gco='git checkout'

    [[ -f $HOME/.config/git/git-prompt.sh ]] && \
	. $HOME/.config/git/git-prompt.sh
    [[ -f $HOME/.config/git/git-completion.bash ]] && \
	. $HOME/.config/git/git-completion.bash

    ___git_complete g __git_main
    ___git_complete gco _git_checkout

    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true

    PS1="[\[\e[00;93m\]\u@\h\[\e[00m\] \[\e[01;01m\]\W\[\e[00m\]\[\e[0;00m\]"'$(__git_ps1 " %s")'"\[\e[0m\]]\$ "
else
    PS1="[\[\e[00;93m\]\u@\h\[\e[00m\] \[\e[01;01m\]\W\[\e[00m\]\[\e[0;00m\]\[\e[0m\]]\$ "
fi

complete -cf sudo
alias sudo='sudo -E'
alias ls='ls --color=auto'
alias ssh='TERM=xterm-256color ssh'
alias ec='emacsclient -c'

export PATH="$HOME/.local/bin:$HOME/bin:$HOME/.local/share/JetBrains/Toolbox/scripts:$PATH"

[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

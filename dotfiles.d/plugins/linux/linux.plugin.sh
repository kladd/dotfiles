#
# linux.plugin.sh
#

if uname|grep "Linux" > /dev/null; then
    alias ls='ls --color=auto'

    # Pacman specific aliases
    which pacman 2> /dev/null 1> /dev/null && {
        alias pacupd='sudo pacman -Syy'
        alias pacupg='sudo pacman -Syu'
        alias pacin='sudo pacman -S'
        alias pacre='sudo pacman -R'
        alias pacs='pacman -Ss'
        alias pacq='pacman -Qs'

        # yaourt aliases
        which yaourt > /dev/null && {
            alias yas='yaourt -Ss'
            alias yain='yaourt -S'
        }

        alias pacclean='sudo pacman -R $(pacman -Qqdt)'
    }

    # apt aliases
    which apt-get 2> /dev/null 1> /dev/null && {
        alias aptupd='sudo apt-get update'
        alias aptupg='sudo apt-get upgrade'
        alias aptup='sudo apt-get udate && sudo apt-get upgrade'
        alias aptin='sudo apt-get install'
        alias aptre='sudo apt-get remove'
        alias apts='apt-cache search'
        alias aptclean='sudo apt-get autoremove'
    }

    [[ -f /etc/bash_completion ]] && . /etc/bash_completion
    [[ -f /usr/share/git/completion/git-prompt.sh ]] && \
        . /usr/share/git/completion/git-prompt.sh
    [[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]] && \
        . /usr/share/git-core/contrib/completion/git-prompt.sh
	[[ -d $HOME/.cabal/bin ]] && \
		prepend_path $HOME/.cabal/bin
fi

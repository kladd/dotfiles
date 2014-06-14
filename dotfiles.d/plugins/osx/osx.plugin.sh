#
# osx.plugin.sh
#

if uname|grep "Darwin" > /dev/null; then
    prepend_path $(brew --prefix)/bin
    prepend_path $(brew --prefix)/share/npm/bin
    prepend_path "/Applications/VMWare Fusion.app/Contents/Library"

    [[ -f $(brew --prefix)/etc/bash_completion ]] && {
        . $(brew --prefix)/etc/bash_completion
    }

    alias ls='ls -G'
fi

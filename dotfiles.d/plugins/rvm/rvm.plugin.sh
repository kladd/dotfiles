#
# rvm.plugin.sh
#

if [ -s "$HOME/.rvm/scripts/rvm" ]; then
    source "$HOME/.rvm/scripts/rvm"
    prepend_path "$HOME/.rvm/bin"
fi

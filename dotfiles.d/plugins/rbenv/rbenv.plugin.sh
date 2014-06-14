#
# rbenv.plugin.sh
#

prepend_path "${HOME}/.rbenv/bin"
prepend_path "${HOME}/.rbenv/plugins/ruby-build/bin"

eval "$(rbenv init -)"

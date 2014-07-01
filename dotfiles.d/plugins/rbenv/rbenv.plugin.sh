#
# rbenv.plugin.sh
#

if [ -d "${HOME}"/.rbenv ]; then
	prepend_path "${HOME}/.rbenv/bin"
	prepend_path "${HOME}/.rbenv/plugins/ruby-build/bin"

	eval "$(rbenv init -)"
fi

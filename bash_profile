#
# bash_profile
#

DOTFILES_DIR=${HOME}/.dotfiles.d

dotfiles=(
		'bash_aliases'
		'bash_prompt'
		'local_bashrc'
		)

plugins=(
		'linux'
		'osx'
		'git'
		'rbenv'
		'barracuda'
		'breach'
		)

function dotfiles()
{
	function dotfiles_update()
	{
		source $DOTFILES_DIR/.dotfiles
		pushd $INSTALL_DIR > /dev/null
		git fetch origin master && git rebase FETCH_HEAD
		./install.sh
		popd > /dev/null
	}

	function dotfiles_reload()
	{
		dotfiles_update
		source $HOME/.bash_profile
	}

	function dotfiles_usage()
	{
		echo "Usage: dotfiles <command>"
		echo ""
		echo "Commands:"
		echo "  update              Update dotfiles"
		echo "  reload              Reload dotfiles"
	}

	case "$1" in
		update)
			dotfiles_update
			;;
		reload)
			dotfiles_reload
			;;
		*)
			dotfiles_usage
			;;
		esac

		unset dotfiles_update
		unset dotfiles_reload
		unset dotfiles_usage
}

function import()
{
	[ -r $1 ] && [ -f $1 ] && source $1
}

function domain()
{
	if [ -e /etc/resolv.conf ]; then
		echo "$(cat /etc/resolv.conf|grep domain|awk -F ' ' '{print $2}')"
	fi
}

function prepend_path()
{
	export PATH=$1:$PATH
}

shopt -s nocaseglob;
shopt -s histappend;
shopt -s cdspell;
shopt -s checkwinsize

for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null;
done;

export HISTCONTROL=ignoredups

complete -cf sudo

prepend_path $HOME/bin
prepend_path $HOME/android/sdk/tools
prepend_path $HOME/android/sdk/platform-tools

# Import plugins
for plugin in ${plugins[@]}; do
	PLUGIN_DIR="${DOTFILES_DIR}/plugins/${plugin}"
	import "${PLUGIN_DIR}/${plugin}.plugin.sh"
done

# Import other dotfiles
for file in ${dotfiles[@]}; do
	import "${HOME}/.${file}"
done

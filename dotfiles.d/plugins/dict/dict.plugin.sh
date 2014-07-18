#
# Uses dict.org to define words on the command line
#

DICT_URL=dict.org
DICT_PORT=2628

if which nc > /dev/null; then
	function define()
	{
		echo "DEFINE wn $@" | nc ${DICT_URL} ${DICT_PORT}
	}
fi

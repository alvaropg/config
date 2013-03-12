#!/bin/sh
if [ -f Makefile ]; then
	echo "Making make maintainer-clean..."
	make maintainer-clean
fi
echo "Removing autogenned files..."
rm -f config.guess config.sub configure install-sh missing mkinstalldirs Makefile.in ltmain.sh stamp-h.in */Makefile.in ltconfig stamp-h config.h.in depcomp aclocal.m4 autom4te.cache
echo "Done."

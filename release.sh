#!/bin/bash -
#===============================================================================
#
#          FILE: release.sh
#
#         USAGE: ./release.sh
#
#   DESCRIPTION: See usage
#
#       OPTIONS: ---
#  REQUIREMENTS: bumper, stack. Must use hpack package.yaml.
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Julian K. Arni,
#  ORGANIZATION:
#       CREATED: 11.07.2016 13:57
#      REVISION:  ---
#===============================================================================

set -o nounset
set -o errexit

git diff-index --quiet HEAD -- || (echo "uncommited changes - exiting"; exit 1)

usage () {
    echo " release.sh <POSITION> "
    echo "             | [-h|--help]"
    echo ""
    echo "    Takes the following steps, in order:"
    echo "      1) Runs the test suite. "
    echo "      2) Bumps the specified version position."
    echo "      3) Substitutes every occurrence of '#SINCE#' in .hs files for "
    echo "         'Since <version number>'."
    echo "      4) Commits the changes."
    echo "      5) Tags the release."
    echo "      6) Releases the package to hackage."
    echo ""
    echo "    POSITION is a number between 0 and 3, inclusive."
    exit 0
}

while [ "${1:-unset}" != "unset" ] ; do
    case "$1" in
        -h | --help) usage ;;
        -d | --dry-run) DRY_RUN=true
           shift ;;
        0) if POSITION="none" ; then POSITION=0 ; else usage ; fi
           shift ;;
        1) if POSITION="none" ; then POSITION=1 ; else usage ; fi
           shift ;;
        2) if POSITION="none" ; then POSITION=2 ; else usage ; fi
           shift ;;
        3) if POSITION="none" ; then POSITION=3 ; else usage ; fi
           shift ;;
        *) usage ;;
   esac
done


bumper -"$POSITION"

VERSION=$( grep "^version:" package.yaml | awk '{ print $2 }')

find . -name '.hs' | xargs -0 sed -i "s/#SINCE#/Since $VERSION/g"

git commit -a .
git tag "$VERSION"
git push && git push --tags
stack upload

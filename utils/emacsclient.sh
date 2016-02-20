#!/bin/bash
# Time-stamp: <2013-12-13 12:02:11 Friday by Yu Yang>
#===============================================================================
#      Filename:  emacsclient.sh
#       Created:  2013-12-12 19:42:45
#
#   DESCRIPTION:  shell for emacsclient
#
#        Author:  Yu Yang
#         Email:  yy2012cn@NOSPAM.gmail.com
#===============================================================================
set -o nounset
ec=emacsclient
emacsclient_installed=$(which emacsclient)
if [ -z "$emacsclient_installed" ];then
    ec=emacsclient.emacs-snapshot
fi

export LC_CTYPE=zh_CN.UTF-8
# emacsclient -a "" -c $*
if ! $ec -c $* ; then
    /usr/bin/env emacs $*
fi

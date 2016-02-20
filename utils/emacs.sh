#!/bin/bash
# Time-stamp: <2013-12-13 11:48:58 Friday by Yu Yang>
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

export LC_CTYPE=zh_CN.UTF-8
/usr/bin/env emacs $*

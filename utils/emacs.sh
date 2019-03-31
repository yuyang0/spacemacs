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

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

export LC_CTYPE=zh_CN.UTF-8
exec /usr/bin/env emacs $*

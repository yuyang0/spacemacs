#!/bin/bash
#==========================================================================
#      Filename:  init.sh
#       Created:  2016-02-22 Mon 09:44
#
#   DESCRIPTION:
#
#        Author:  Yu Yang
#         Email:  yy2012cn@NOSPAM.gmail.com
#==========================================================================

# for golang
go get -u github.com/gpmgo/gopm
go get golang.org/x/tools/cmd/goimports
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef

# fuck GFW
gopm get -u -v golang.org/x/tools/cmd/oracle
gopm get -u -v golang.org/x/tools/cmd/gorename

# for python
sudo apt-get install -y python-pip
sudo pip install  jedi==0.8.1 json-rpc==1.8.1 service_factory==0.1.2
sudo pip install wakatime

# for javascript
sudo apt-get install -y nodejs npm
sudo npm install -g tern js-beautify jshint

# for c/c++
sudo apt-get install clang

#! /bin/sh
emacs --daemon
emacsclient -c --alternate-editor=$0

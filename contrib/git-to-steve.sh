#!/bin/sh
#
# 
USER_EMAIL=$(git config user.email)
if [ -z "$USER_EMAIL" ]; then
    echo "You need to setup your email address with:"
    echo "    git config user.email <your email address>"
    exit 1
fi
USER_NAME=$(git config user.name)
if [ -z "$USER_NAME" ]; then
    echo "You need to configure git with your name:"
    echo "    git config user.name \"John Doe\""
    exit 1
fi
git branch --track for-steve origin/master
git checkout for-steve

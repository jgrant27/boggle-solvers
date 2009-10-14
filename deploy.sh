#!/usr/bin/env bash

rsync \
    -avz \
    --delete \
    --exclude=boggle/gen-boards.sh \
    --exclude=boggle/deploy.sh \
    --exclude=boggle/.git \
    --exclude=boggle/*~ \
    --exclude=boggle/bin/IMAGES_ARE_SAVED_HERE \
    --exclude=boggle/bin/gen-boards \
    --exclude=boggle/bin/solve-boards \
    --exclude=boggle/src/*~ \
    --exclude=boggle/dict/*~ \
    --exclude=boggle/src/ruby \
    --exclude=boggle/src/lisp/*~ \
    ~/boggle $1:.

ssh $1 "pushd /home/$2/boggle ; ./make.sh ; rm -fr src/ ; rm -fr make.sh"

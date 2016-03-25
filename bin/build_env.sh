#!/bin/bash

BUILD_PATH=/opt/build

export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$BUILD_PATH/lib/pkgconfig
export XDG_DATA_DIRS=$XDG_DATA_DIRS:$BUILD_PATH/share:/usr/share:/usr/local/share
export GI_TYPELIB_PATH=$GI_TYPELIB_PATH:$BUILD_PATH/lib/girepository-1.0

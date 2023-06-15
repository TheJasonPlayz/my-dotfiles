#! /usr/bin/env bash

stowdir=$(pwd)/../stow-dir

cd $stowdir/home

stow . -t ~/
#!/bin/bash

for libdir in $(ls -d $(dirname $0)/libs/* )
do 
    bash -c "cd $libdir ; git remote -v" | awk '{print $2}'
done | sort | uniq

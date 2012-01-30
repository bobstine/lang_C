#!/bin/bash

for ((i=1; i<6; i++))
do
    for ((d=0; d<10; d++))
    do
	./bellman --gamma $i.$d --rounds 1000
    done
done
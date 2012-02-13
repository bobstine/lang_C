#!/bin/bash

for gamma in 0.1 0.25 0.5 0.60 0.62 0.64 0.75 0.9 1 1.1 1.25 1.5 1.75 2 2.25 2.5 3 3.5 4 5 7 9 11 14 20 30 50
do
    ./bellman --gamma $gamma --rounds $2 --prob $1 --spend 0.5 
done
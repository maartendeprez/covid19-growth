#!/bin/bash

regions=( China South-Korea Italy Spain Belgium Netherlands Brazil Switzerland France Germany US UK )
#regions=( China South-Korea Italy Belgium Brazil US )

stack run -- --source worldometers --minimum 10 --smoothing 7 --graph active "${regions[@]}" > graphs/active.html
stack run -- --source worldometers --minimum 10 --smoothing 7 --graph confirmed "${regions[@]}" > graphs/confirmed.html
stack run -- --source worldometers --minimum 2 --smoothing 7 --graph deaths "${regions[@]}" > graphs/deaths.html
stack run -- --source worldometers --minimum 10 --smoothing 7 --avg-smoothing 3 --graph --daily confirmed "${regions[@]}" > graphs/confirmed-daily.html
stack run -- --source worldometers --minimum 2 --smoothing 7 --avg-smoothing 3 --graph --daily deaths "${regions[@]}" > graphs/deaths-daily.html

scp graphs/*.html crissaegrim.be.eu.org:covid19/

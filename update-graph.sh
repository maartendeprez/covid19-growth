#!/bin/bash

regions=( China South-Korea Italy Spain Belgium Netherlands Brazil Switzerland France Germany US UK )
# Romania not yet available

stack run -- --source worldometers --minimum 10 --smoothing 7 --graph confirmed "${regions[@]}" > confirmed.html
stack run -- --source worldometers --minimum 10 --smoothing 7 --graph active "${regions[@]}" > active.html
stack run -- --source worldometers --minimum 2 --smoothing 7 --graph deaths "${regions[@]}" > deaths.html

scp *.html crissaegrim.be.eu.org:covid19/

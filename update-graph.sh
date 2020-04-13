#!/bin/bash

europe=( Italy Spain Belgium Netherlands Romania Switzerland Austria France Germany Sweden Norway Finland UK )
america=( Brazil Chile Argentina Ecuador Bolivia Mexico US Canada )
africa=( South-Africa Democratic-Republic-of-the-Congo Egypt Israel Palestine )
rest=( China South-Korea Japan Russia India Australia Iran Iraq )
brstates=( CE RJ SP DF MG GO )

minimum=100
mindeaths=10
smoothing=3

stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph active "${europe[@]}" > graphs/active-europe.html
stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph confirmed "${europe[@]}" > graphs/confirmed-europe.html
stack run -- --source worldometers --minimum $mindeaths --smoothing $smoothing --graph deaths "${europe[@]}" > graphs/deaths-europe.html

stack run -- --source csse --minimum $minimum --smoothing $smoothing --graph active "${europe[@]}" > graphs/active-europe-csse.html
stack run -- --source csse --minimum $minimum --smoothing $smoothing --graph confirmed "${europe[@]}" > graphs/confirmed-europe-csse.html
stack run -- --source csse --minimum $mindeaths --smoothing $smoothing --graph deaths "${europe[@]}" > graphs/deaths-europe-csse.html

stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph active "${america[@]}" > graphs/active-america.html
stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph confirmed "${america[@]}" > graphs/confirmed-america.html
stack run -- --source worldometers --minimum $mindeaths --smoothing $smoothing --graph deaths "${america[@]}" > graphs/deaths-america.html

stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph active "${africa[@]}" > graphs/active-africa.html
stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph confirmed "${africa[@]}" > graphs/confirmed-africa.html
stack run -- --source worldometers --minimum $mindeaths --smoothing $smoothing --graph deaths "${africa[@]}" > graphs/deaths-africa.html

stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph active "${rest[@]}" > graphs/active-rest.html
stack run -- --source worldometers --minimum $minimum --smoothing $smoothing --graph confirmed "${rest[@]}" > graphs/confirmed-rest.html
stack run -- --source worldometers --minimum $mindeaths --smoothing $smoothing --graph deaths "${rest[@]}" > graphs/deaths-rest.html

stack run -- --source sus --minimum $minimum --smoothing $smoothing --graph confirmed "${brstates[@]}" > graphs/confirmed-brasil.html
stack run -- --source sus --minimum $mindeaths --smoothing $smoothing --graph deaths "${brstates[@]}" > graphs/deaths-brasil.html

scp graphs/*.html crissaegrim.be.eu.org:covid19/

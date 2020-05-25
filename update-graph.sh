#!/bin/bash

europe=( Italy Spain Belgium Netherlands Romania Switzerland Austria France Germany Sweden Norway Finland UK )
america=( Brazil Chile Peru Argentina Ecuador Bolivia Colombia Mexico US Canada )
africa=( South-Africa Democratic-Republic-of-the-Congo Ghana Egypt Israel )
rest=( China South-Korea Japan Russia India Australia Iran Iraq Turkey )
brstates=( CE RJ SP DF MG GO AM PE BA )
beprovinces=( VlaamsBrabant Limburg Antwerpen WestVlaanderen )
bemunis=( Scherpenheuvel-Zichem Holsbeek Aarschot Kortrijk Herselt Wervik )

minactive=100
minconfirmed=100
mindeaths=10
mindailyactive=100
mindailyconfirmed=100
mindailydeaths=10
minhospitalizations=10
mindailyhospitalizations=10

smoothing=7
average=7


stack build
graph=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/corona/corona


# Worldometer country growth rate graphs

for item in active confirmed deaths; do

    for region in europe america africa rest; do

	echo "Graphing $region $item..."

	$graph --source worldometers --minimum $(eval echo \$min$item) --smoothing $smoothing --graph $item $(eval echo \${$region[@]}) > graphs/$item-$region.html
	$graph --source worldometers --daily --minimum $(eval echo \$mindaily$item) --smoothing $smoothing --average $average --graph $item $(eval echo \${$region[@]}) > graphs/$item-$region-daily.html

    done

done


# Brasil per-state graphs confirmed cases

echo "Graphing Brasil confirmed..."

$graph --source sus --minimum $minconfirmed --smoothing $smoothing --graph confirmed "${brstates[@]}" > graphs/confirmed-brasil.html
$graph --source sus --daily --minimum $mindailyconfirmed --smoothing $smoothing --average $average --graph confirmed "${brstates[@]}" > graphs/confirmed-brasil-daily.html
$graph --source sus --absolute --minimum $minconfirmed --smoothing 1 --graph confirmed "${brstates[@]}" > graphs/confirmed-brasil-absolute.html
$graph --source sus --absolute --daily --minimum $mindailyconfirmed --smoothing 1 --average 2 --graph confirmed "${brstates[@]}" > graphs/confirmed-brasil-absolute-daily.html


# Brasil per-state graphs deaths

echo "Graphing Brasil deaths..."

$graph --source sus --minimum $mindeaths --smoothing $smoothing --graph deaths "${brstates[@]}" > graphs/deaths-brasil.html
$graph --source sus --daily --minimum $mindailydeaths --smoothing $smoothing --average $average --graph deaths "${brstates[@]}" > graphs/deaths-brasil-daily.html
$graph --source sus --absolute --minimum $mindeaths --smoothing $smoothing --graph deaths "${brstates[@]}" > graphs/deaths-brasil-absolute.html
$graph --source sus --absolute --daily --minimum $mindailydeaths --smoothing $smoothing --average $average --graph deaths "${brstates[@]}" > graphs/deaths-brasil-absolute-daily.html


# Belgium per-province hospitalizations

echo "Graphing Belgium hospitalizations..."

$graph --source sciensano --minimum $minhospitalizations --smoothing $smoothing --graph TOTAL_IN "${beprovinces[@]}" > graphs/hospitalizations-belgium.html
$graph --source sciensano --daily --minimum $mindailyhospitalizations --smoothing $smoothing --graph TOTAL_IN "${beprovinces[@]}" > graphs/hospitalizations-belgium-daily.html

$graph --source sciensano --minimum $minhospitalizations --smoothing $smoothing --average $average --graph TOTAL_IN_ICU "${beprovinces[@]}" > graphs/hospitalizations-icu-belgium.html
$graph --source sciensano --daily --minimum $mindailyhospitalizations --smoothing $smoothing --average $average --graph TOTAL_IN_ICU "${beprovinces[@]}" > graphs/hospitalizations-icu-belgium-daily.html

# Belgium per-municipality cases

echo "Graphing Belgium cases..."

$graph --source sciensano --absolute --minimum 1 --smoothing 1 --graph CASES "${bemunis[@]}" > graphs/belgium-cases-absolute.html
$graph --source sciensano --minimum 1 --smoothing $smoothing --graph CASES "${bemunis[@]}" > graphs/belgium-cases-growth.html

scp graphs/*.html crissaegrim.be.eu.org:covid19/

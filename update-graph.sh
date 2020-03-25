#!/bin/bash

regions=( Hubei,China "South Korea" Italy Spain Belgium Netherlands Brazil Romania "New York,US" )

for item in confirmed active deaths; do
    stack run -- --source worldometers --graph $item "${regions[@]}" > $item.html
done

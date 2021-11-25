#!/bin/sh

s=$(curl -v $HOST:$PORT'/list/indices' | sed 's/,/ /g' | sed 's/"//g')

for i in ${s:1:-1}

  do curl -v $HOST:$PORT'/ms-plot?cache=false&index='$i --output /dev/null

done

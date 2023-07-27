#!/bin/bash

i="all"

while getopts ":f:e:i::" flag; do
case $flag in
f) f=${OPTARG} ;;
e) e=${OPTARG} ;;
i) i=${OPTARG} ;;
esac
done

set -a

source ./$e

set +a

BRANCH=$(git symbolic-ref --short -q HEAD)

if [ "$BRANCH" != "main" ]; then

HOST=$DEV_HOST
DB_PASSWORD=$DEV_DB_PASSWORD
DB_PRIMARY_PASSWORD=$DEV_DB_PRIMARY_PASSWORD
DB_ROOT_PASSWORD=$DEV_DB_ROOT_PASSWORD

fi

if [ $i = "volume" ]; then

ITEM=".items[0]"

elif [ $i = "image" ]; then

ITEM=".items[1]"

elif [ $i = "build" ]; then

ITEM=".items[2]"

elif [ $i = "deploy-app" ]; then

ITEM=".items[3]"

elif [ $i = "deploy-db" ]; then

ITEM=".items[4]"

elif [ $i = "service-app" ]; then

ITEM=".items[5]"

elif [ $i = "service-db" ]; then

ITEM=".items[6]"

elif [ $i = "route" ]; then

ITEM=".items[7]"

elif [ $i = "job" ]; then

ITEM=".items[8]"

else

  ITEM=""

fi

oc process -f $f \
-p BRANCH=$BRANCH \
-p HOST=$HOST \
-p DB_PASSWORD=$DB_PASSWORD \
-p DB_PRIMARY_PASSWORD=$DB_PRIMARY_PASSWORD \
-p DB_ROOT_PASSWORD=$DB_ROOT_PASSWORD \
-p FINBIF_ACCESS_TOKEN=$FINBIF_ACCESS_TOKEN \
-p FINBIF_API=$FINBIF_API \
-p FINBIF_WAREHOUSE_QUERY=$FINBIF_WAREHOUSE_QUERY \
-p FINBIF_EMAIL=$FINBIF_EMAIL \
-p TIMEOUT_IN_HOURS=$TIMEOUT_IN_HOURS \
| jq $ITEM

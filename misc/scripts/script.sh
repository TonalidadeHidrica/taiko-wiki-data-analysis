#!/bin/bash

set -eux

mkdir -p /tmp/taiko-wiki/a /tmp/taiko-wiki/b

while read line
do
  rm -f /tmp/taiko-wiki/a/* /tmp/taiko-wiki/b/*

  line=($line)
  id=${line[0]}
  filename=${line[1]}
  year=${filename:16:4}
  month=${filename:20:2}
  day=${filename:22:2}
  hour=${filename:25:2}
  minute=${filename:27:2}
  second=${filename:29:2}
  date="${year}-${month}-${day}T${hour}:${minute}:${second}Z"

  if [ -e taiko-wiki/wiki ]
  then
    rm -r taiko-wiki/wiki
  fi
  gdrive download $id --path /tmp/taiko-wiki/a/
  unzip /tmp/taiko-wiki/a/$filename -d /tmp/taiko-wiki/b/
  tar zxf /tmp/taiko-wiki/b/$filename -C taiko-wiki/ --strip-components 1 dump-data/wiki
  git -C taiko-wiki add wiki
  if ! git -C taiko-wiki diff-index --quiet HEAD -- 
  then
    git -c user.name="bot" -c user.email="<>" -C taiko-wiki \
      commit --author="bot <>" --message="Wiki snapshot as of ${date}"
  fi
done < list5.txt

#!/bin/bash

set -eux

for full_file in /tmp/taiko-wiki/zips/*.zip
do
  filename=$(basename $full_file)
  echo $filename

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
  unzip -q $full_file -d taiko-wiki/ "wiki/*"
  git -C taiko-wiki add wiki
  if ! git -C taiko-wiki diff-index --quiet HEAD -- 
  then
    git -c user.name="bot" -c user.email="<>" -C taiko-wiki \
      commit --author="bot <>" --message="Wiki snapshot as of ${date}"
  fi
done

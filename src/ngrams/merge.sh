#!/bin/sh
set -e
pv 1980 | goog_clean > o
pv o | LC_ALL=C sort -k 2 > o2
pv o2 | goog_merge > o3
pv o3 | LC_ALL=C sort -nr > 1980l

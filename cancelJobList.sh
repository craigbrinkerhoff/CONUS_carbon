#!/bin/bash
for j in `seq 3396825 3396856` ; do
    scancel $j
    echo  $j
done

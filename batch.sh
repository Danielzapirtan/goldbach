#! /bin/bash

n="$1"

for s in $(seq 1 8); do
	t=$(($n - 8 + $s))
	gh workflow run Goldbach -f c=$t
	echo $t
	sleep 5
done
echo "All workflows triggered"

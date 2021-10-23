#!/bin/bash
file_location=test.txt
test1=aa
test2=bb
if [ -e text.txt ]; then
  echo "File already exists!"
else
  cat > $file_location <<EOF
==============================
Output differences from simulation
$test1
compared to:
$test2
==============================

EOF
fi


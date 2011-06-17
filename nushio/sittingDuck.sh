#!/bin/sh
opp()
{
  read lr
  case $lr in
    1) read card; read slot;;
    2) read slot; read card;;
  esac
}
if [ $1 = "1" ]; then
  opp
fi
while [ true ]; do
  echo "1"
  echo "I"
  echo "0"
  opp
done

]

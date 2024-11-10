#!/bin/bash

# Milos Prokop, 2024, free to use and modify

gatebase=cliffordt

dim_lb=$1 
dim_ub=$2

b=$3 #qubits per dim

echo $gatebase
echo "$dim_lb - $dim_ub"
echo "b=$b"

#for dim in {$((dim_lb))..$((dm_ub))}
for ((dim=$((dim_lb)); dim <= $((dim_ub)) ; dim++))
do
    time=$(date)
    echo "Starting evaluation of dim=$((dim)) at $time"
    OUT_GATECOUNT=$(./src/svp -f GateCount -n $dim -m $dim -g $gatebase -b $b | tac |  sed -n '4 p' | cut -d " " -f 3)
    OUT_DEPTH=$(./src/svp -f ascii -n $dim -m $dim -g $gatebase -b $b | quipper-depth )
    echo "Gatecount: $OUT_GATECOUNT" >&2
    echo "$OUT_DEPTH" >&2
done

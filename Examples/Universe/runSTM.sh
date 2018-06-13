#!/bin/sh
# Job name
#SBATCH -J universe-stm
#
# output file
#SBATCH -o universe-stm-%j.txt
#
# Project ID
#SBATCH -A project00625
#
# Request the time you need for execution in [hour:]minute
#SBATCH -t 00:30:00
#
# Required resources
#SBATCH -C avx&mpi
#
# Request vitual memory you need for your job in MB
#SBATCH --mem-per-cpu 128
#
# Number of tasks
#SBATCH -n 1
# Number of Nodes
#SBATCH -N 1
# CPUs per task for multi-threaded tasks
#SBATCH -c 16
# request exclusive access
#SBATCH --exclusive

module unload openmpi
module load java
echo "--------- processors ------------------------"
#cat /proc/cpuinfo
lscpu
echo "--------- java version ----------------------"
java -version
echo "---------------------------------------------"

export LANG=en_US.UTF-8
export JAVA_OPTS="-Xmx1024m -Xms1024m -DengineName=stm"
./target/universal/stage/bin/run-console

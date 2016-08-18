#!/bin/sh
# Job name
#BSUB -J REScalaBenchmark
#
# File / path where STDOUT will be written, the %J is the job id
#BSUB -o parrp-%J.out
#
# Request the time you need for execution in [hour:]minute
#BSUB -W 23:30
#
# Required resources
#BSUB -R "select[ mpi && avx ]"
#
# Request vitual memory you need for your job in MB
#BSUB -M 2048
#
# Request the number of compute slots you want to use
#BSUB -n 16
#BSUB -q deflt_auto
# request exclusive access
#BSUB -x

module unload openmpi
module load java
echo "--------- processors ------------------------"
cat /proc/cpuinfo
echo "--------- java version ----------------------"
java -version
echo "---------------------------------------------"

export LANG=en_US.UTF-8
export JAVA_OPTS="-Xmx1024m -Xms1024m -DengineName=parrp"
./target/start

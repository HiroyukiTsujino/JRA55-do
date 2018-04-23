#!/bin/sh
#
#-------------------------------------
echo "Original links are removed."
#------------------------------------
rm -f data
rm -f forcing
rm -f result
#-------------------------------------
#
expname="EXP"
#
hostname=`hostname -s`
loginname=`logname`
#
case $hostname in
front* )
  basedir_data=/home1/oc
  basedir_forcing=/work/oc
  basedir_result=/short/oc
  ;;
ocsv011 )
  basedir_data=/work111
  basedir_forcing=/work111
  basedir_result=/work111
  ;;
* )
  echo "unknown hostname : $hostname"
  exit 1 
esac

datadir=$basedir_data/$loginname/$expname/data    
forcingdir=$basedir_forcing/$loginname/$expname/forcing
resultdir=$basedir_result/$loginname/$expname/result 

if [ -e $datadir ] ; then
    ln -s $datadir data
else
    mkdir -p $datadir 
    ln -s $datadir data
    echo "${datadir} is not found"
    echo "${datadir} is created."
fi      
if [ -e $forcingdir ] ; then
    ln -s $forcingdir forcing
else
    mkdir -p $forcingdir
    ln -s $forcingdir forcing
    echo "${forcingdir} is not found"
    echo "${forcingdir} is created."
fi      
if [ -e $resultdir ] ; then
    ln -s $resultdir result
else
    mkdir -p $resultdir
    ln -s $resultdir result
    echo "${resultdir} is not found"
    echo "${resultdir} is created."
fi      
echo "linkdir/data:    ${datadir}"
echo "linkdir/forcing: ${forcingdir}"
echo "linkdir/result : ${resultdir}"








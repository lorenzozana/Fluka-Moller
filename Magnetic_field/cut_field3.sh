file=`echo $1`
i=0
scale=5
rm info_file.txt
head -n 5 $1 > info_file.txt

while read line ; do
    if [ "$i" -eq "0" ]
    then
	nx=`echo $line | awk '{printf("%s \n",$1)}'`
	xlow=`echo $line | awk '{printf("%s \n",$2)}'`
	xhigh=`echo $line | awk '{printf("%s \n",$3)}'`
	stepx=` echo "($xhigh -($xlow) )/($nx  - 1) * $scale" | bc -l |  awk '{printf("%.3f\n",$1)}'`
#	echo $stepx
    fi
    if [ "$i" -eq "1" ]
    then
	ny=`echo $line | awk '{printf("%s \n",$1)}'`
	ylow=`echo $line | awk '{printf("%s \n",$2)}'`
	yhigh=`echo $line | awk '{printf("%s \n",$3)}'`
	stepy=` echo "($yhigh -($ylow) )/($ny  - 1) * $scale" | bc -l |  awk '{printf("%.0f\n",$1)}'`
#	echo $stepy
    fi
    if [ "$i" -eq "2" ]
    then
	nz=`echo $line | awk '{printf("%s \n",$1)}'`
	zlow=`echo $line | awk '{printf("%s \n",$2)}'`
	zhigh=`echo $line | awk '{printf("%s \n",$3)}'`
	stepz=` echo "($zhigh -($zlow) )/($nz  - 1) * $scale" | bc -l |  awk '{printf("%.1f\n",$1)}'`
#	echo $stepz
    fi
    
#    if [ $i -gt 5 ] 
#    then
#	j=`expr $i % 100`
#	if [ "$j" -eq "0" ] ;
#	then echo $line
#	fi
#    fi
    ((i=i+1)) 
done < info_file.txt 

for x in $(seq $xlow $stepx $xhigh)
do
#    echo $x $y $z
#    echo $x
    x=`echo $x | gawk  '{sub(/\./,"\\\.",$0);printf($1)}'`
#    echo $x
    for y in -25 -20 -15 -10 -5 0 5 10 15 20 25
    do
#	echo $y
	y=`echo "\t"$y`
#	echo $y
	for z in  6 6.5 7 7.5 8
	do
	    z=`echo "\t"$z"\s" | gawk  '{sub(/\./,"\\\.",$0);printf($1)}'`
#	    echo $x $y $z
	    string=`echo $x$y$z`
#	    echo $string
#	    grep -P "0\.015\s" blockyHybrid_rm_3.0.txt | grep -P "\t9\s" | grep -P "\t9\.5\s"
#	    grep -P "$x" $file | grep -P "$y" | grep -P "$z"
	    grep -P "$string" $file
	done
    done   
done

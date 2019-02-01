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
	echo $stepx
    fi
    if [ "$i" -eq "1" ]
    then
	ny=`echo $line | awk '{printf("%s \n",$1)}'`
	ylow=`echo $line | awk '{printf("%s \n",$2)}'`
	yhigh=`echo $line | awk '{printf("%s \n",$3)}'`
	stepy=` echo "($yhigh -($ylow) )/($ny  - 1) * $scale" | bc -l |  awk '{printf("%.0f\n",$1)}'`
	echo $stepy
    fi
    if [ "$i" -eq "2" ]
    then
	nz=`echo $line | awk '{printf("%s \n",$1)}'`
	zlow=`echo $line | awk '{printf("%s \n",$2)}'`
	zhigh=`echo $line | awk '{printf("%s \n",$3)}'`
	stepz=` echo "($zhigh -($zlow) )/($nz  - 1) * $scale" | bc -l |  awk '{printf("%.1f\n",$1)}'`
	echo $stepz
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
    for y in $(seq $ylow $stepy $yhigh)
    do
	for z in $(seq $zlow $stepz $zhigh)
	do
	    echo $x $y $z
	    grep "$x " $file | grep " $y " | grep " $z "
	done
    done   
done

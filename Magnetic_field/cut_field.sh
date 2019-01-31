file=`echo $1`
i=0
while read line ; do
    if [" $i" -eq "0" ]
    then
	nx=`echo $line | awk '{printf("%s \n",$1)}'`
	xlow=`echo $line | awk '{printf("%s \n",$2)}'`
	xhigh=`echo $line | awk '{printf("%s \n",$3)}'`
    fi
    if [" $i" -eq "1" ]
    then
	ny=`echo $line | awk '{printf("%s \n",$1)}'`
	ylow=`echo $line | awk '{printf("%s \n",$2)}'`
	yhigh=`echo $line | awk '{printf("%s \n",$3)}'`
    fi
    if [" $i" -eq "2" ]
    then
	nz=`echo $line | awk '{printf("%s \n",$1)}'`
	zlow=`echo $line | awk '{printf("%s \n",$2)}'`
	zhigh=`echo $line | awk '{printf("%s \n",$3)}'`
    fi

    
    if [ $i -gt 5 ] 
    then
	j=`expr $i % 100`
	if [ "$j" -eq "0" ] ;
	then echo $line
	fi
    fi
    ((i=i+1)) 
done < $file 


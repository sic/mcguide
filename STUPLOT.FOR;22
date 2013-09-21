*	A program to plot the output from a Monte Carlo simulation
*	Stuart Campbell (21/11/96)

	Program plot_mc

	parameter(dp=1000)
	character*40 filename, name
	character wav, titley, errtitle
	real xmin, xmax, ymin, ymax
	real x(dp), y(dp), e(dp), ylo(dp), yhi(dp)

1	continue

        write(6,*) 'Enter file to be plotted :'
	read(5,99) filename

	open(unit=10,file=filename,status='old',err=1)	
                  
	          
        read(10,99) name
        read(10,*) 
*wav, titley, errtitle
        read(10,*)
                  
	xmin = 1.0e20
	ymin = 1.0e20
        
	xmax = -1.e20
	ymax = -1.e20

	do i = 1,10000
	read(10,*,err=2) x(i), y(i), e(i)
	enddo
	
2	continue
	n = i - 1

	write(6,101)
	read(5,*) xmin,xmax,ymin,ymax

	do i = 1, n
             ylo(i) = y(i) - e(i)/2
	     yhi(i) = y(i) + e(i)/2
	enddo

* Now lets do the plotting stuff!
	
        call pgbegin(0, '?', 1, 1)
        call pgslw(2)
	call pgenv(xmin, xmax, ymin, ymax, 0,  1)
C	call pgenv(0, 30, 0, ymax, 0,  1)
	
	call pgpoint(n, x, y, 9)
	call pgerry(n, x, ylo, yhi, 5.0)
	call pgask(.false.)
	call pgend

	close(10)

99	format(a40)
100	format(A) 
101	format('Enter limits for plot {XMIN,XMAX,YMIN,YMAX}  :')

	stop 
	end

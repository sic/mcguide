*	This program simulates a time of flight neutron spectrometer
*	Author: David Martin / Stuart Campbell
	
	program spectra

	integer totwaves, totimes
	parameter (totwaves=100, totimes=100)
	integer guideopt, ntimes, nwav
	real wav(totwaves), wgxe(totwaves), sgxe(totwaves) 
	real sample(totwaves, totimes), exit(totwaves, totimes)  
	real upx(totwaves), dwx(totwaves)
	real up(totwaves, totimes), down(totwaves, totimes)  
	real syme(totwaves), ratio(totwaves, totimes)
	real fille(totwaves), illu(totwaves, totimes)
	real ftranse(totwaves), trans(totwaves, totimes)
	real delole(totwaves), delta(totwaves, totimes)
	real avrefe(totwaves), reflex(totwaves, totimes)
	real upsum, upmean(totwaves), arg_up, upvar,up_variance(totwaves)
	real updev(totwaves)
	real downsum, downmean(totwaves), arg_down, downvar
	real down_variance(totwaves), downdev(totwaves)
	real sume, arg_evare, evar
	real emean(totwaves), edev(totwaves), evariance(totwaves)
	real sum, arg_var, var
	real smean(totwaves), dev(totwaves), variance(totwaves)
	real sumr, rat_arg_var, rat_var
	real rmean(totwaves), rat_dev(totwaves), rat_variance(totwaves)
	real sumi, illu_arg_var, illu_var
	real imean(totwaves), illu_dev(totwaves), illu_variance(totwaves)
	real sumt, trans_arg_var, trans_var
	real tmean(totwaves), trans_dev(totwaves), trans_variance(totwaves)
	real sumd, delta_arg_var, delta_var
	real dmean(totwaves), delta_dev(totwaves), delta_variance(totwaves)
	real sumx, reflex_arg_var, reflex_var
	real xmean(totwaves), reflex_dev(totwaves), reflex_variance(totwaves)
	real g05cbf, g05ccf, g05daf
	character*40 name
	external mcguide
	
	open(unit=10, file='options.in', status='old')
	open(unit=15, file='end.out', status='new')
	open(unit=20, file='final.out', status='new')
	open(unit=30, file='ratio.out', status='new')
	open(unit=40, file='illumin.out', status='new')
	open(unit=50, file='trans.out', status='new')
	open(unit=60, file='deltalol.out', status='new')
	open(unit=70, file='reflect.out', status='new')
        open(unit=80, file='up.out', status='new')
        open(unit=90, file='down.out', status='new')
        
	read(10,*) name
	read(10,*)
	read(10,*) guideopt, ntimes

	close(10)

	if(guideopt.eq.1)then

*	initializing random number generator with a repeteable seed.
  		call g05cbf(0)

*	simulating neutron flux at guide exit and sample plane.
	   	do j = 1, ntimes


	      		call mcguide(nwav,wav,wgxe,sgxe,syme,
     *				    fille,ftranse,delole,avrefe,
     *				    upx,dwx)
	        	do i = 1, nwav
				up(i,j) = upx(i)
				down(i,j) = dwx(i)
				exit(i,j) = wgxe(i)
		      		sample(i,j) = sgxe(i)
				ratio(i,j) = syme(i)
				illu(i,j) = fille(i)
				trans(i,j) = ftranse(i)	
				delta(i,j) = delole(i)
				reflex(i,j) = avrefe(i)			
			enddo
		enddo	

*	calculating mean values and standard deviation.
		do i = 1, nwav	
			sume = 0.
			vare = 0.
			sum = 0.
			var = 0.
			sumr = 0.
			rat_var = 0.
			sumi = 0.
			illu_var = 0.
			sumt = 0.
			trans_var = 0.
			sumd = 0.
			delta_var = 0.
			sumx = 0.
			reflex_var = 0.
			upsum = 0.
			downsum = 0.
			upvar = 0.
			downvar = 0.

			do j = 1, ntimes
				upsum = up(i,j) + upsum
				downsum = down(i,j) + downsum
				sume  = exit(i,j) + sume
				sum  = sample(i,j) + sum
				sumr = ratio(i,j)  + sumr
				sumi = illu(i,j)   + sumi
				sumt = trans(i,j)  + sumt
				sumd = delta(i,j)  + sumd
				sumx = reflex(i,j) + sumx
			enddo

			upmean(i) = upsum/ntimes
			downmean(i) = downsum/ntimes

			emean(i) = sume /ntimes
			smean(i) = sum /ntimes
			rmean(i) = sumr/ntimes
			imean(i) = sumi/ntimes
			tmean(i) = sumt/ntimes
			dmean(i) = sumd/ntimes
			xmean(i) = sumx/ntimes

        		do j = 1, ntimes
				arg_up = up(i,j)-upmean(i)
				upvar = arg_up*arg_up + upvar
			enddo
			up_variance(i) = (1./(ntimes-1.))*upvar
			updev(i) = sqrt(up_variance(i))
                        
			do j = 1, ntimes
				arg_down = down(i,j)-downmean(i)
				downvar = arg_down*arg_down + downvar
			enddo
			down_variance(i) = (1./(ntimes-1.))*downvar
			downdev(i) = sqrt(down_variance(i))
                        


			do j = 1, ntimes
				arg_evar = exit(i,j)-emean(i)
				evar = arg_evar*arg_evar + evar
			enddo
			evariance(i) = (1./(ntimes-1.))*evar
			edev(i) = sqrt(evariance(i))

        		do j = 1, ntimes
				arg_var = sample(i,j)-smean(i)
				var = arg_var*arg_var + var
			enddo
			variance(i) = (1./(ntimes-1.))*var
			dev(i) = sqrt(variance(i))
                        
        		do j = 1, ntimes
				rat_arg_var = ratio(i,j)-rmean(i)
				rat_var = rat_arg_var*rat_arg_var + rat_var
			enddo
			rat_variance(i) = (1./(ntimes-1.))*rat_var
			rat_dev(i) = sqrt(rat_variance(i))

        		do j = 1, ntimes
				illu_arg_var = illu(i,j)-imean(i)
				illu_var = illu_arg_var*illu_arg_var + illu_var
			enddo
			illu_variance(i) = (1./(ntimes-1.))*illu_var
			illu_dev(i) = sqrt(illu_variance(i))

        		do j = 1, ntimes
				trans_arg_var = trans(i,j)-tmean(i)
				trans_var = trans_arg_var*trans_arg_var + 
     *				trans_var
			enddo
			trans_variance(i) = (1./(ntimes-1.))*trans_var
			trans_dev(i) = sqrt(trans_variance(i))

        		do j = 1, ntimes
				delta_arg_var = delta(i,j)-dmean(i)
				delta_var = delta_arg_var*delta_arg_var + 
     *				delta_var
			enddo
			delta_variance(i) = (1./(ntimes-1.))*delta_var
			delta_dev(i) = sqrt(delta_variance(i))

        		do j = 1, ntimes
				reflex_arg_var = reflex(i,j) - xmean(i)
				reflex_var = reflex_arg_var*reflex_arg_var + 
     *				reflex_var
			enddo
			reflex_variance(i) = (1./(ntimes-1.))*reflex_var
			reflex_dev(i) = sqrt(reflex_variance(i))

        	enddo

*	writing results.

		write(15,100)name
		write(15,215)
		write(15,*)                            
*		write(15,*) nwav
		write(15,315) (wav(i), emean(i), edev(i), i = 1, nwav)
                
		write(80,100) name
		write(80,225)
		write(80,*)                            
		write(80,315) (wav(i), upmean(i), updev(i), i = 1, nwav)
                
 		write(90,100) name
 		write(90,226)
  		write(90,*)                            
 		write(90,315) (wav(i), downmean(i), downdev(i), i = 1, nwav)
                



		write(20,100)name
		write(20,220)
		write(20,*)                            
*		write(20,*) nwav
		write(20,320) (wav(i), smean(i), dev(i), i = 1, nwav)

		write(30,100)name
		write(30,230)
		write(30,*)                            
*		write(30,*) nwav
		write(30,330) (wav(i), rmean(i), rat_dev(i), i = 1, nwav)

		write(40,100)name
		write(40,240)
		write(40,*)                            
*		write(40,*) nwav
		write(40,340) (wav(i), imean(i), illu_dev(i), i = 1, nwav)

		write(50,100)name
		write(50,250)
		write(50,*)                            
*		write(50,*) nwav
		write(50,350) (wav(i), tmean(i), trans_dev(i), i = 1, nwav)

		write(60,100)name
		write(60,260)
		write(60,*)                            
*		write(60,*) nwav
		write(60,360) (wav(i), dmean(i), delta_dev(i), i = 1, nwav)

		write(70,100)name
		write(70,270)
		write(70,*)                            
*		write(70,*) nwav
		write(70,370) (wav(i), xmean(i), reflex_dev(i), i = 1, nwav)

        endif

	close(15)
	close(20)
	close(30)
	close(40)
	close(50)
	close(60)
	close(70)
	close(80)
	close(90)

100	format(a40)
215	format(4x, 'wavelength', 9x, ' n/sec @ exit ', 7x, 'std. dev.')
220	format(4x, 'wavelength', 9x, 'n/sec @ sample', 7x, 'std. dev.')
225	format(4x, 'wavelength', 9x, 'up/sec @ exit', 7x, 'std. dev.')
226	format(4x, 'wavelength', 9x, 'down/sec @ exit', 7x, 'std. dev.')
230	format(4x, 'wavelength', 9x, 'ratio @ sample', 7x, 'std. dev.')
240	format(4x, 'wavelength', 9x, 'illumination', 7x, 'std. dev.')
250	format(4x, 'wavelength', 9x, 'transmission ', 7x, 'std. dev.')
260	format(4x, 'wavelength', 9x, '    dl/l     ', 7x, 'std. dev.')
270	format(4x, 'wavelength', 9x, 'reflections', 7x, 'std. dev.')
315	format(5x, f7.3, 11x, e13.6, 8x, e8.1)
320	format(5x, f7.3, 11x, e13.6, 8x, e8.1)
330	format(5x, f7.3, 15x, f7.3, 11x, f7.3)
340	format(5x, f8.4, 12x, f8.4, 11x, f8.4)
350	format(5x, f7.3, 12x, f7.3, 11x, f7.3)
360	format(5x, f7.3, 9x, e13.6, 9x, e8.1)
370	format(5x, f7.3, 14x, f7.3, 11x, f7.3)

	stop
	end


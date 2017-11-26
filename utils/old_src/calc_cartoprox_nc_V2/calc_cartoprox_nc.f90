      program calc_cartoprox_nc
!****************************************************
!*     PREVALP/CARTOPROX                            *
!*     Calcul des concentration CARTPROX            *
!*     date : decembre 2010                         *
!*          auteur: E. Chaxel                       *
!*		       LEGI/GIERSA	            *
!*			chaxeleric@yahoo.fr         *
!****************************************************

      use netcdf
      use typesizes

      implicit none 

      character(len=256),parameter :: program_exe='calc_cartoprox_nc.exe'
      character(len=256) :: fileIn, fileOut     
      
      integer :: pointDimID, frTimeDimID, xVarID, ncFileOutID, ncFileID    
      integer :: numPoints, numFrTimes      
      
      integer,parameter :: nvar = 5      
      character(len=20),parameter :: varstr(nvar) = (/ 'NO', 'NO2', 'O3', 'PM10', 'PM25'/)
      real,parameter :: ppb2microg(nvar) = (/ 1.21, 1.91, 1.99, 1., 1./)      
           
      character(len=20) :: varstr_sirane
      character(len=20) :: varstr_grille        
      character(len=20) :: varstr_fond         
      
      real, allocatable :: conc_sirane(:)
      real, allocatable :: conc_grille(:)      
      real              :: conc_fond(1) 
      
      integer :: start1d(1)
      integer :: start2d(2)   
      
      integer :: it, iv, ip   

!--------------------------------------------------------------------------------------------      
      logical :: isirane, ifond, igrille  

      call getarg(1,fileIn)
      call getarg(2,fileOut)      

      write(*,*) 'Lit le fichier NetCDF cartoprox '//trim(fileIn)      
      !Fichier ENTREES      
      call check(nf90_open(path = trim(fileIn), mode = nf90_nowrite, ncid = ncFileID))       
      call check(nf90_inq_dimid(ncFileID,'Point', pointDimID))     
      call check(nf90_inq_dimid(ncFileID, 'Time', frTimeDimID))                      
      call check(nf90_Inquire_Dimension(ncFileID, pointDimID , len= numPoints))             
      call check(nf90_Inquire_Dimension(ncFileID, frTimeDimID, len= numFrTimes))
      
      ! Allocation
      allocate(conc_sirane(numPoints))
      allocate(conc_grille(numPoints)) 
            
      !Fichier SORTIES
      call check(nf90_open(fileOut, nf90_write  , ncFileOutID))       
                              	  	           
      do it = 1, numFrTimes
      
        start2d=(/  1,  it/)
        start1d=(/	it/)	  
      
        do iv = 1, nvar
        
         !Defini les variables
         varstr_sirane = trim(adjustl(varstr(iv)))
         varstr_grille = trim(adjustl(varstr(iv)))//'_grille'	
         varstr_fond  = trim(adjustl(varstr(iv)))//'_fond'

         isirane=.false.
         igrille=.false.
         ifond=.false.
         		           
         !la variable SIRANE est dans le fichier ?
	 if ( nf90_inq_varid(ncFileID, varstr_sirane, xVarID).eq.nf90_noerr) then  
	   
           isirane=.true.
           if ( nf90_inq_varid(ncFileID, varstr_grille, xVarID) .eq. nf90_noerr)igrille=.true.
           if ( nf90_inq_varid(ncFileID, varstr_fond, xVarID) .eq. nf90_noerr)ifond=.true.
          
           if (ifond.and.igrille.and.isirane) then
        		
           call check(nf90_inq_varid(ncFileID, varstr_fond, xVarID) )   
           call check(nf90_get_var(ncFileID, xVarID, conc_fond, count=(/1/), start=start1d ) ) 

           call check(nf90_inq_varid(ncFileID, varstr_grille, xVarID) )
           call check(nf90_get_var(ncFileID, xVarID, conc_grille(:),count=(/numPoints/), start=start2d) )	
        	   
           call check(nf90_inq_varid(ncFileID, varstr_sirane, xVarID) )
           call check(nf90_get_var(ncFileID, xVarID, conc_sirane(:),count=(/numPoints/), start=start2d ) )
        
           ! Formule CARTOPROX
           do ip = 1, numPoints
             conc_sirane(ip) = conc_sirane(ip) - conc_fond(1) + conc_grille(ip) * ppb2microg(iv)
           end do
	   
	   if(it.eq.1)write(*,*)trim(varstr_sirane)//'-> '//trim(fileOut)   
           call check(nf90_inq_varid(ncFileOutID, varstr_sirane, xVarID) )	   
	   call check(nf90_put_var(ncFileOutID, xVarID, conc_sirane,count=(/numPoints/),start=start2d) )
	   
	   else	   
             write(*,*) 'Manque une variable'
	     write(*,*) 'sirane=', isirane
	     write(*,*) 'grille=', igrille
	     write(*,*) 'fond='  , ifond
           end if	      
           
         end if !Variable dans le fichier
             
        end do
   
      end do 
            
      call check(nf90_close(ncFileID)) 
      call check(nf90_close(ncFileOutID))       

      write(*,*) '-> '//trim(fileOut)
      write(*,*) 'Fin du programme '//trim(program_exe)
                    
      end program
!*****************************************************************************      
subroutine check(status)
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 

  use netcdf
  use typesizes

  implicit none

  integer, intent ( in) :: status
   
  if(status .ne. nf90_noerr) then 
    print*, 'Error occured'
    print *, 'status=',status
    print*, trim(nf90_strerror(status))
    stop    
  end if
end subroutine check  


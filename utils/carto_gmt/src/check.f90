!*****************************************************************************      
subroutine check(status)
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 
  integer, intent ( in) :: status
  character(len = 80)  :: nf90_strerror  
  if(status /= 0) then     
!  if(status /= nf90_noerr) then 
    print*, 'Error occured'
    print *, 'status=',status
    stop    
  end if
end subroutine check  


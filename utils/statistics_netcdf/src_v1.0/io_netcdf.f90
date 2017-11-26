   subroutine lit_date_cdf
   
      use netcdf
      use typesizes 
      use params
      
      implicit none   
             
      call check(nf90_open(fout1, nf90_write, out1FileID))  
      call check(nf90_inq_varid(out1FileID, 'Times', xVarID))    
      call check(nf90_get_var(out1FileID, xVarID, datestr, start = (/1,it/) ))      
      call check(nf90_close(out1FileID))       
      
   end subroutine lit_date_cdf

         

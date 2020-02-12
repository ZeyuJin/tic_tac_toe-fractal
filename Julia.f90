program Julia
   implicit none
   real, dimension(:,:), allocatable :: dat
   real :: x1, x2, y1, y2, dx, dy, zr, zi
   real, parameter :: pi = 3.1415926
   real, dimension(4) :: boundary
   integer :: nx, ny, ix, iy, it, idat, ndat, i
   integer, dimension(2) :: nxny
   integer, parameter :: num = 13 ! create animated GIF images
   complex :: c0, c, z

   print *, 'Enter the boundary and interval: '
   print *, 'Enter x1, x2, y1, y2, nx, ny'
   read *, x1, x2, y1, y2, nx, ny

   dx = (x2 - x1) / real(nx)
   dy = (y2 - y1) / real(ny)
   allocate(dat(num,nx*ny))

   do i = 1, num
      ! Julia sets with quadratic polynomials
      ! this example could be found in wikipedia
      c0 = cmplx(0, pi/6.0*(i-1))
      c = 0.7885 * exp(c0)

      idat = 0
      do ix = 1, nx
         do iy = 1, ny
            zr = x1 + dx/2.0 + dx*real(ix - 1)
            zi = y1 + dy/2.0 + dy*real(iy - 1)
            z = cmplx(zr,zi)

            do it = 1, 10000
               z = z*z + c
               if (cabs(z) > 2) exit  
            enddo
            idat = idat + 1
            dat(i,idat) = it
         enddo
      enddo
   enddo

   print *, 'Finished Calculation'
   ndat = idat*num
   print *, 'The number of points', ndat

   open(12, file = 'julia.bin', form = 'unformatted')
   write(12) num

   nxny(1) = nx
   nxny(2) = ny
   write(12) nxny

   boundary(1) = x1
   boundary(2) = x2
   boundary(3) = y1
   boundary(4) = y2
   write(12) boundary

   write(12) dat
   close(12)

end program Julia

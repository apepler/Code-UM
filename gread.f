      program gread

c----------------------------------------------------------------------
c
c     SUBROUTINE: gread
c
c     FILE: gread
c
c     LANGUAGE: f90
c
c     DESCRIPTION:
c
c     The subroutine of gauscheck that is responsible for the
c     reading of the input ausmap type files.
c
c     HISTORY:
c
c     WRITTEN: D.A. Jones
c     REVISED: 
c     LAST CHANGE: 25/09/2000.
c
c----------------------------------------------------------------------

      implicit none

      integer idem
      parameter(idem=500)

      integer iunit,ie,clength
      integer nx,ny,i,j,h

      real yl(idem),xl(idem)
      real data(idem,idem)
      character ihead*200,ifile*80


      ifile="monthfile"
      iunit=1
      open(iunit,file=ifile,status='old',form='unformatted',err=110)
      ie=0
      read(iunit,err=10,end=20) ny
      print *, ny
      if(ny.GT.idem) stop 'INCREASE IDEM IN gread'
      read(iunit,err=10,end=20)yl(1:ny)
      print *, minval(yl), maxval(yl)
      read(iunit,err=10,end=20)nx
      print *, nx
      if(nx.GT.idem) stop 'INCREASE IDEM IN gread'
      read(iunit,err=10,end=20)xl(1:nx)
      print *, minval(xl),maxval(xl)
      read(iunit,err=10,end=20)ihead
      print *, ihead
      read(iunit,err=10,end=20)data(1:nx,1:ny)
      return

10    write (6,'(/2a/)')'ERROR READING INPUT FILE: ',ifile
      ie=2
20    write(6,'(/2a/)')'END OF FILE REACHED: ',ifile
      ie=1
110   print *, "Could not open file"
      end

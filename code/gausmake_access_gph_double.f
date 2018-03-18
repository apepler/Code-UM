      module mod_inc
         integer ilon,jlat,alon,alat,ilev,level
         parameter (alon=500)
         parameter (alat=500)
         parameter (ilev=22)
         real*8 latitude2(alat)
         real*8 longitude2(alon)
         real latitude(alat)
         real longitude(alon)
         character*10 gtype
         character*8 vunits,vtype
         character*5 rtype,spec
         character*4 xname,yname,vname
         save
      end      
      
      program gausmake

c----------------------------------------------------------------------
c
c     PROGRAM: GAUSMAKE
c
c     FILE: gausmake.f
c
c     LANGUAGE: f90
c
c
c     A program for the reading of a series of fields from ncep data
c     and converting them into the ausmap format.
c     Start/end date and time are taken from the file ingausmake.
c     Header details are also supplied in ingausmake
c     The output is written in the ausmap format to the file tape11.
c
c     The output is compatible with the gcycloc program 
c     and is specifically designed for the lat/lon 
c     version of the automatic cyclone finding and tracking 
c     package.
c
c     Currently designed for NCEP slp grids.
c     Assumes names in netcdf are 'slp','lat','lon','time'
c
c     USAGE:
c
c     gausmake
c
c     EXPLANATION:
c
c
c     The following information is supplied by the instruction file.
c
c     quant       Defines variable and level of data.
c     source      Source of identification code for data.
c     unit        Units for data.
c     ilon,jlat Length of longitude/latitude
c
c     The instruction file ("instfile") specifies the allowable 
c     sequence of data times and some field parameters.
c
c     dystrt,hrstrt Starting date and time
c     dystop,hrstop Finishing date and time
c
c     HISTORY:
c
c     Created: A.S. Pepler
c     Based on gauscheck (D.A. Jones)
c     LAST CHANGE: 15/08/2013 
c
c----------------------------------------------------------------------

      use mod_inc
      implicit none

      integer dystrt,hrstrt,dystop,hrstop
      integer dylast,hrlast,hours,ndays,n
      integer ny,nm,nd,nh,hint,yy1,mm1,dd1,yy,yd,dlen
      integer i,j,k,iunit,icode,inet,inet2,inet3,latv,lonv
      integer ncopn,ncvid,utopen 

      character*80 instfile,ofile,ohead,path,name
      character*200 ifile,ifile2,ifile3
      character*3 member      
      character*80 dum1
      integer dtype,dum2,dum4
      integer dum3(4)


c----------------------------------------------------------------------
c     Variable assignments.
c----------------------------------------------------------------------

      instfile = 'ingausmake'
      ofile    = 'tape11'
c  Assumes a file of format 'name',year,'.nc'. Eg slp.2008.nc
      name='da_zg_'

      write(6,'(/a/)')'PROGRAM: gausmake'
      call get_command_argument(1,spec)
      open(2,file=ofile,form='unformatted') 
      open(3,file=instfile,status='old')

c------------------------------------------------------------
c      Read instruction file.
c------------------------------------------------------------
      read(3,10)vtype,rtype,vunits,gtype,
     & dystrt,ndays,ilon,jlat,member,level,path
      close(3)
10    format (//a8/a5/a8/a10/i8/i3/i3/i3/a3/i3/a80)


c------------------------------------------------------------

c------------------------------------------------------------
c     Finds all dates in file
c------------------------------------------------------------

      write(ifile,'(a,a,i8,a,a3,a3)')trim(path)
     &,trim(name),dystrt,'_',member,'.nc'
      inet=ncopn(ifile,0,icode)
      latv=ncvid(inet,'lat',icode)
      lonv=ncvid(inet,'lon',icode)

      call ncvinq(inet,latv,dum1,dtype,dum2,dum3,dum4,icode)
      if (dtype.eq.5) then
        call ncvgt(inet,latv,1,jlat,latitude,icode)
        call ncvgt(inet,lonv,1,ilon,longitude,icode)
      else
        call ncvgt(inet,latv,1,jlat,latitude2,icode)
        call ncvgt(inet,lonv,1,ilon,longitude2,icode)
        latitude=latitude2
        longitude=longitude2
      endif

cc Load every instance in the file. Figure out its date

      yy1= floor(real(dystrt)/10000.0)
      mm1= mod(floor(real(dystrt)/100.0),100)
      dd1= mod(dystrt,100)

      do n=1,ndays
         call getdays(yy1,mm1,dlen)
         if (dd1.gt.dlen) then
            dd1=1
            mm1=mm1+1
            if (mm1.gt.12) then
                mm1=1
                yy1=yy1+1
            endif
         endif
         call write_ausm(yy1,mm1,dd1,n,inet,2)
         dd1=dd1+1
      enddo
           
      close(2)      
      end program
c-----------------------------------------------------------


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC   Subroutine to load, format & write a date

      subroutine write_ausm(ny,nm,nd,nh,inet,iunit)
      
CC Declare all the internal variables

      use mod_inc
      
      character*80 header
      character*8 cdate
      character*4 ctime
      character tname
      real x(ilon,jlat)
c      real xscale,xoff
      real*8 levD(ilev)
      real levF(ilev)
      integer lev(ilev)
      real*8 x2(ilon,jlat)
      real work(ilon,jlat)
      integer it,ot,tt,tlen,tvar,icode,ny,nm,nd,nh
      integer inet,iunit,ivar,ilon2
      integer*2 miss
      integer start(4)
      integer count(4)
      character*80 dum1
      integer dtype,dum2,dum4
      integer dum3(4)

cc Get the right level. Check if float or double before loading

      ivar=ncvid(inet,'z1_p_level',icode)
      call ncvinq(inet,ivar,dum1,dtype,dum2,dum3,dum4,icode)
      if (dtype.eq.5) then
        call ncvgt(inet,ivar,1,ilev,levF,icode)
        lev=int(levF)
      else
        call ncvgt(inet,ivar,1,ilev,levD,icode)
        lev=int(levD)
      endif      
      
      ll=minloc(lev,1,mask=(lev.eq.level))
c      print *, level, ll, lev(ll) 

CC  Extract and unpack the needed timestep of data      
      
      start(1)=1
      start(2)=1
      start(3)=ll
      start(4)=nh
      count(1)=ilon
      count(2)=jlat
      count(3)=1
      count(4)=1
      
      ivar=ncvid(inet,'zg',icode)
      call ncvinq(inet,ivar,dum1,dtype,dum2,dum3,dum4,icode)     
      if (dtype.eq.5) then
        call ncvgt(inet,ivar,start,count,x,icode)
      else
        call ncvgt(inet,ivar,start,count,x2,icode)
        x=x2
      endif

CC Create the header line
      if (spec.eq.'norm') then
         write(header,1001)
     $   vtype,rtype,ny,nm,nd,0,vunits,gtype
      else if (spec.eq.'spec') then
         write(header,1001)
     $   vtype,rtype,ny-80,nm,nd,0,vunits,gtype
      else
       print *, 'Error - special or normal?'
      endif

 1001 format (A8,22x,A5,5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,A10)


c      print*, header
c      print*,jlat,ilon,latitude(jlat),longitude(ilon),maxval(x)

CC Write to the file that's currently open     

      write(iunit)jlat
      write(iunit)(latitude(j),j=1,jlat)
      write(iunit)ilon
      write(iunit)(longitude(i),i=1,ilon)
      write(iunit)header
      write(iunit)((x(i,j),i=1,ilon),j=1,jlat)
      return
      end


********************************************************************
CC    These two subroutines (unpack and xhour) are borrowed directly from NCEP
CC    http://www.esrl.noaa.gov/psd/data/gridded/readgeneral.F

      subroutine unpack(x,z,xscale,xadd,miss,ilon,jlat)
      parameter (zmiss=1e30)

      integer*2 x(ilon,jlat),miss
      real y(ilon,jlat)
      real z(ilon,jlat)

      do i=1,ilon
         do j=1,jlat
            if(x(i,j).eq.miss)then
               y(i,j)=zmiss
            else
              y(i,j)=(x(i,j)*xscale)+xadd
           endif
         enddo
      enddo

      z = y/100


      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine xhour(iy,im,id,ih,xhours)

      integer iy,im,id,ih	! The input year, month, day and hour
      integer*4 xhours		! OUT # of hours between in and 1-1-1

      integer*4 xdays		! Used in calculating hours
      integer   inyear		! Used to work with input year

      integer imonth(12)	! Used in calculating # days in this year
      integer imonthl(12)	! "					"

      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
      data imonthl/31,29,31,30,31,30,31,31,30,31,30,31/

C     See if date is in 1900s but given as 2 digit.

      if (iy.lt.100) then
         inyear = iy + 1900
      else
         inyear = iy
      endif

C     CALCULATE DAYS FROM JAN 1 Year 1

      xdays=0
      xhours=0

      xdays = INT((inyear-1)*365.25)

      if(im.gt.1)then
        do imm=1,im-1
           if((mod(inyear,4).eq.0).and.(inyear.ne.0))then
             xdays=xdays+imonthl(imm)
           else
             xdays=xdays+imonth(imm)
           endif
        enddo
      endif

      xdays=xdays+id

      xhours = xdays*24

      xhours = xhours + ih

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getdays(iy,im,id)

      integer iy,im,id  	! The input year, month & output days

      integer imonth(12)	! Used in calculating # days in this year
      integer imonthl(12)	! "					"

      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
      data imonthl/31,29,31,30,31,30,31,31,30,31,30,31/

C     Is leapyear? 

      if((mod(iy,4).eq.0).and.(mod(iy,100).ne.0))then
             id=imonthl(im)
      else if(mod(iy,400).eq.0) then       
             id=imonthl(im)
      else
             id=imonth(im)
      end if

      return
      end


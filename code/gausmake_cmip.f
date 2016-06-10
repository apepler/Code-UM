      module mod_inc
         integer ilon,jlat,alon,alat,jlat2
         parameter (alon=500)
         parameter (alat=500)
         real latitude(alat)
         real longitude(alon)
         character*30 csource
         character*10 gtype
         character*8 vunits,vtype,vname
         character*5 rtype,spec
         integer inet1,inet2,cmip,divisor,reverse
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
c
c     This version removes the highest and lowest latitudes 
c     since they weren't in the output of gcycloc from BoM
c
c     This version also swaps the latitude order, for consistency
c     with ausmap. Not sure if will make a difference. 
c
c     This is a very messy version that runs on the ERAI data from 
c     Alejandro's stuff - two files of 1989-1999 and 2000-2010
c     As well as three smaller files in my folder
c     So no one else should use this... 
c
c
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
c     LAST CHANGE: 26/09/2013 

c
c   NOTE: ALL CMIP models are set to skip leap years, to make my life easier
c
c----------------------------------------------------------------------

      use mod_inc
      implicit none

      integer dystrt,hrstrt,dystop,hrstop
      integer dylast,hrlast,hours
      integer ny,nm,nd,nh,hint,yr1,yr2,mm1,mm2,yy,yd,dlen,dd1
      integer i,j,k,iunit,icode,latv,lonv
      integer ncopn,ncvid,utopen
      character*80 instfile,ofile,ohead,name,dir
      character*200 ifile1,ifile2
      


c----------------------------------------------------------------------
c     Variable assignments.
c----------------------------------------------------------------------

      instfile = 'ingausmake'
      ofile    = 'tape11'
c  Will have to explicitly define filenames later in the program
c  And open both to start with. Then call from one based on year.

      write(6,'(/a/)')'PROGRAM: gausmake'
      call get_command_argument(1,spec)
      open(2,file=ofile,form='unformatted') 
      open(3,file=instfile,status='old')

c------------------------------------------------------------
c      Read instruction file.
c------------------------------------------------------------
      read(3,10)vtype,rtype,vunits,gtype,
     & dystrt,hrstrt,dystop,hrstop,hint,cmip
      close(3)
10    format (//a8/a5/a8/a10/i8/i2/i8/i2/i2/i1)

c-----------------------------------------------------------
c      Open the two files
c------------------------------------------------------------
      dir='/srv/ccrc/data41/z3444417/Data/CMIP3/'

      if (cmip.eq.1) then
        write(ifile1,'(a,a)')trim(dir)
     &  ,'MIROC3.2/1990-2010/MIROC3.2_6H_1990-2010_slp.nc'
        write(ifile2,'(a,a)')trim(dir)
     &  ,'MIROC3.2/2060-2080/MIROC3.2_6H_2060-2080_slp.nc'
        vname='slp'
        divisor=1
        ilon=128
        jlat=64
        reverse=0
      else if (cmip.eq.2) then
        write(ifile1,'(a,a)')trim(dir)
     &  ,'ECHAM5/1990-2010/ECHAM5_6H_1990-2010_slp.nc'
        write(ifile2,'(a,a)')trim(dir)
     &  ,'ECHAM5/2060-2080/ECHAM5_6H_2060-2080_slp.nc'
        vname='slp'
        divisor=100
        ilon=192
        jlat=96
        reverse=0
      else if (cmip.eq.3) then
        write(ifile1,'(a,a)')trim(dir)
     &  ,'CCCMA/1990-2010/CCCMA3.1_6H_1990-2010_psl.nc'
        write(ifile2,'(a,a)')trim(dir)
     &  ,'CCCMA/2060-2080/CCCMA3.1_6H_2060-2080_psl.nc'
        vname='psl'
        divisor=100
        ilon=96
        jlat=48
        reverse=1
      else if (cmip.eq.4) then
        write(ifile1,'(a,a)')trim(dir)
     &  ,'CSIROMk3.0/1990-2010/CSIROMk3.0_6H_1990-2010_slp.nc'
        write(ifile2,'(a,a)')trim(dir)
     &  ,'CSIROMk3.0/2060-2080/CSIROMk3.0_6H_2060-2080_slp.nc'
        vname='slp'
        divisor=1
        ilon=96
        jlat=48
        reverse=1
      else
         print *, 'Only four CMIP3 models'
         stop
      endif  

      inet1=ncopn(ifile1,0,icode)
      inet2=ncopn(ifile2,0,icode)

      latv=ncvid(inet1,'lat',icode)
      call ncvgt(inet1,latv,1,jlat,latitude,icode)
      lonv=ncvid(inet1,'lon',icode)
      call ncvgt(inet1,lonv,1,ilon,longitude,icode)  

c------------------------------------------------------------
c     Convert start & end date to list of dates
c------------------------------------------------------------
cc Convert into yr1, mm1 & yy2, mm2

      yr1= floor(real(dystrt)/10000.0)
      mm1= mod(floor(real(dystrt)/100.0),100)
      yr2= floor(real(dystop)/10000.0)
      mm2= mod(floor(real(dystop)/100.0),100)
      dd1= mod(real(dystop),100.0)
           
cc Open the ERA files for yr0,yr1,yr2
cc Do do-loop on months, within which you check # days
cc Then do loop on days,hours to save ausmap

      yd=yr2-yr1
      if (yd.eq.0) then
         call getdays(yr1,mm1,dlen)
         do j=dd1,dlen
            do k=1,hint
               nh=(24/hint)*(k-1)
               call write_ausm(yr1,i,j,nh,2)
            enddo 
         enddo
         mm1=mm1+1
         do i=mm1,mm2
            call getdays(yr1,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr1,i,j,nh,2)
              enddo 
            enddo
         enddo  
         
      else if (yd.eq.1) then 
               
cc Loops twice - once for each year

         do i=mm1,12
            call getdays(yr1,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr1,i,j,nh,2)
              enddo 
            enddo
         enddo         
         do i=1,mm2
            call getdays(yr2,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr2,i,j,nh,2)
              enddo 
            enddo
         enddo  
         
      else if (yd.eq.2) then     

cc Assumes the longest possible season in run_gcyc
cc one year + month on either side (e.g. 20091201-20110101)
         yy=yr1+1
         
         do j=1,31
            do k=1,hint
              nh=(24/hint)*(k-1)
              call write_ausm(yr1,12,j,nh,2)
            enddo 
         enddo
         do i=1,12
            call getdays(yy,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yy,i,j,nh,2)
              enddo 
            enddo
         enddo         
         do j=1,31
            do k=1,hint
              nh=(24/hint)*(k-1)
              call write_ausm(yr2,1,j,nh,2)
            enddo 
         enddo         
      else
         print *, 'Maximum period: 14 months'
         stop
      endif               

      close(2)      
      end program
c-----------------------------------------------------------


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC   Subroutine to load, format & write a date

      subroutine write_ausm(ny,nm,nd,nh,iunit)
      
CC Declare all the internal variables

      use mod_inc
      
      character*100 header
      character tname
      real x(ilon,jlat)
      real x2(ilon,jlat)
      real*8 xscale,xoff
      real latitude2(jlat)
      real longitude2(ilon)
      real work(ilon,jlat)
      integer it,ot,tt,tlen,tvar,icode,ny,nm,nd,nh
      integer inet,sy,iunit,ivar
      integer*2 miss
      integer start(3)
      integer count(3)

CC  Everything depends on which is the right file for ERAI
CC  So identify which is the right file to use & declare ot
CC  Everything else should be same!

      if (ny.le.2010) then
        sy=1990
        inet=inet1
      else
        sy=2060
        inet=inet2
      endif

      call xhour(ny,nm,nd,nh,it)
      call xhour(sy,1,1,0,ot)
      tt = (it-ot)/6+1

      start(1)=1
      start(2)=1
      start(3)=tt
      count(1)=ilon
      count(2)=jlat
      count(3)=1
      ivar=ncvid(inet,trim(vname),icode)
      call ncvgt(inet,ivar,start,count,work,icode)
c      call ncagt(inet,ivar,'scale_factor',xscale,icode)
c      call ncagt(inet,ivar,'add_offset',xoff,icode)
c      call ncagt(inet,ivar,'missing_value',miss,icode)
      
c      call unpack(work,x,xscale,xoff,miss,ilon,jlat)
      x=work/divisor

CC Create the header line      

CC Create the header line      
      if (spec.eq.'norm') then 
         write(header,1001)
     $   vtype,rtype,ny,nm,nd,100*nh,vunits,gtype
      else if (spec.eq.'spec') then 
         write(header,1001)
     $   vtype,rtype,ny-80,nm,nd,100*nh,vunits,gtype
      else
       print *, 'Error - special or normal?'
      endif
 1001 format (A8,22x,A5,5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,A10)

c      print *,latitude(1),latitude(jlat),longitude(1)
c     &,longitude(ilon),work(1,1),x(1,1)

CC Write to the file that's currently open    

      if(reverse.eq.0) then 
        longitude2=longitude(1:ilon:1)
        latitude2=latitude(jlat:1:-1)
        x2=x(:,jlat:1:-1)
      else
        longitude2=longitude(1:ilon:1)
        latitude2=latitude(1:jlat:1)
        x2=x
      endif

      write(iunit)jlat
      write(iunit)(latitude2(j),j=1,jlat)
      write(iunit)ilon
      write(iunit)(longitude2(i),i=1,ilon)
      write(iunit)header
      write(iunit)((x2(i,j),i=1,ilon),j=1,jlat)
      
      return
      end


********************************************************************
CC    These two subroutines (unpack and xhour) are borrowed directly from NCEP
CC    http://www.esrl.noaa.gov/psd/data/gridded/readgeneral.F

      subroutine unpack(x,z,xscale,xadd,miss,ilon,jlat)
      parameter (zmiss=1e30)
      real*8 xscale,xadd
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
             if((mod(inyear,100).eq.0).and.(mod(inyear,400).ne.0))then
                  xdays=xdays+imonth(imm)
             else
                  xdays=xdays+imonthl(imm)
             endif
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
c      subroutine getdays(iy,im,id)
c
c      integer iy,im,id  	! The input year, month & output days
c
c      integer imonth(12)	! Used in calculating # days in this year
c      integer imonthl(12)	! "					"
c
c      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
c      data imonthl/31,29,31,30,31,30,31,31,30,31,30,31/
c
C     Is leapyear? 
c
c      if((mod(iy,4).eq.0).and.(mod(iy,100).ne.0))then
c             id=imonthl(im)
c      else if(mod(iy,400).eq.0) then       
c             id=imonthl(im)
c      else
c             id=imonth(im)
c      end if
c
c      return
c      end
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getdays(iy,im,id) !NOTE - NO LEAP YEARSc

      integer iy,im,id  	! The input year, month & output daysc

      integer imonth(12)	! Used in calculating # days in this year				"

      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/

      id=imonth(im)

      return
      end


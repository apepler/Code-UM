      module mod_inc
         integer ilon,jlat,alon,alat,jlat2
         parameter (alon=600)
         parameter (alat=600)
         real latitude(alat)
         real longitude(alon)
         character*30 csource
         character*10 gtype
         character*8 vunits,vtype
         character*5 rtype
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
c----------------------------------------------------------------------

      use mod_inc
      implicit none

      integer dystrt,hrstrt,dystop,hrstop
      integer dylast,hrlast,hours
      integer ny,nm,nd,nh,hint,yr1,yr2,mm1,mm2,yy,yd,dlen
      integer i,j,k,iunit,icode,latv,lonv
      integer ncopn,ncvid,utopen,inet

      character*80 instfile,ofile,ohead,name
      character*250 ifile,path
      


c----------------------------------------------------------------------
c     Variable assignments.
c----------------------------------------------------------------------

      instfile = 'ingausmake'
      ofile    = 'tape11'
      path = '/srv/ccrc/data28/z3444417/Data/ERAI/' // 
     +       'global/psl/raw/fullres/'
      
c  Assumes a file of format 'EIN',YYYYMM,'_an_MSLP.nc.' 
      name='_an_MSLP.nc'

      write(6,'(/a/)')'PROGRAM: gausmake'
      open(2,file=ofile,form='unformatted') 
      open(3,file=instfile,status='old')

c------------------------------------------------------------
c      Read instruction file.
c------------------------------------------------------------
      read(3,10)vtype,rtype,vunits,gtype,
     & dystrt,hrstrt,dystop,hrstop,hint,ilon,jlat
      close(3)
10    format (//a8/a5/a8/a10/i8/i2/i8/i2/i2/i3/i3)


c------------------------------------------------------------
c     Convert start & end date to list of dates
c------------------------------------------------------------
cc Convert into yr1, mm1 & yy2, mm2

      yr1= floor(real(dystrt)/10000.0)
      mm1= mod(floor(real(dystrt)/100.0),100)
      yr2= floor(real(dystop)/10000.0)
      mm2= mod(floor(real(dystop)/100.0),100)
           
cc Call the first month's netcdf file to xtract lat & lon      
      write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &,'EIN',yr1,mm1,trim(name)
      inet=ncopn(ifile,0,icode)   
      latv=ncvid(inet,'g0_lat_1',icode)
      call ncvgt(inet,latv,1,jlat,latitude,icode)
      lonv=ncvid(inet,'g0_lon_2',icode)
      call ncvgt(inet,lonv,1,ilon,longitude,icode)  


cc Open the ERA files for yr0,yr1,yr2
cc Do do-loop on months, within which you check # days
cc Then do loop on days,hours to save ausmap

      yd=yr2-yr1
      if (yd.eq.0) then  
         do i=mm1,mm2
            write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &      ,'EIN',yr1,i,trim(name)
            inet=ncopn(ifile,0,icode)   
            call getdays(yr1,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr1,i,j,nh,inet,2)
              enddo 
            enddo
         enddo  
         
      else if (yd.eq.1) then 
               
cc Loops twice - once for each year

         do i=mm1,12
            write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &      ,'EIN',yr1,i,trim(name)
            inet=ncopn(ifile,0,icode)   
            call getdays(yr1,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr1,i,j,nh,inet,2)
              enddo 
            enddo
         enddo         
         do i=1,mm2
            write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &      ,'EIN',yr2,i,trim(name)
            inet=ncopn(ifile,0,icode)   
            call getdays(yr2,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr2,i,j,nh,inet,2)
              enddo 
            enddo
         enddo  
         
      else if (yd.eq.2) then     

cc Assumes the longest possible season in run_gcyc
cc one year + month on either side (e.g. 20091201-20110101)
         yy=yr1+1
         write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &   ,'EIN',yr1,12,trim(name)
         inet=ncopn(ifile,0,icode)            
         do j=1,31
            do k=1,hint
              nh=(24/hint)*(k-1)
              call write_ausm(yr1,12,j,nh,inet,2)
            enddo 
         enddo
         do i=1,12
            write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &      ,'EIN',yy,i,trim(name)
            inet=ncopn(ifile,0,icode)            
            call getdays(yy,i,dlen)
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yy,i,j,nh,inet,2)
              enddo 
            enddo
         enddo
         write(ifile,'(a,a,i4,i2.2,a)')trim(path)
     &   ,'EIN',yr2,1,trim(name)
         inet=ncopn(ifile,0,icode)                     
         do j=1,31
            do k=1,hint
              nh=(24/hint)*(k-1)
              call write_ausm(yr2,1,j,nh,inet,2)
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

      subroutine write_ausm(ny,nm,nd,nh,inet,iunit)
      
CC Declare all the internal variables

      use mod_inc
      
      character*80 header
      character*8 cdate
      character*4 ctime
      character tname
      real x(ilon,jlat)
      real x2(ilon,jlat)
      real xscale,xoff
      real latitude2(jlat)
      integer*2 work(ilon,jlat)
      integer it,ot,tt,tlen,tvar,icode,ny,nm,nd,nh
      integer inet,iunit,ivar
      integer*2 miss
      integer start(3)
      integer count(3)

CC  Identify the point of the netcdf file with the data wanted
CC  Assumes the netcdf file is of a single MONTH with 6-hourly data
      
      call xhour(ny,nm,nd,nh,it)
      call xhour(ny,nm,1,0,ot)
      tt = (it-ot)/6+1
      
CC  Extract and unpack the needed timestep of data      
      
      start(1)=1
      start(2)=1
      start(3)=tt
      count(1)=ilon
      count(2)=jlat
      count(3)=1
      
      ivar=ncvid(inet,'MSL_GDS0_SFC',icode)
      call ncvgt(inet,ivar,start,count,x,icode)
      x=x/100

CC Create the header line      

      write(header,'(A8,22x,A5,5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,A10)')
     $vtype,rtype,ny,nm,nd,100*nh,vunits,gtype

CC Write to the file that's currently open    
      latitude2=latitude(jlat:1:-1)
      x2=x(:,jlat:1:-1)
      write(iunit)jlat
      write(iunit)(latitude2(j),j=1,jlat)
      write(iunit)ilon
      write(iunit)(longitude(i),i=1,ilon)
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


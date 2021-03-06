      module mod_inc
         integer ilon,jlat,alon,alat,jlat2
         parameter (alon=500)
         parameter (alat=500)
         real latitude(alat),longitude(alon)
         character*30 csource
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
c     This version is designed to use WRF regridded to 2.5x2.5 degrees over a limited area
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
c     This version is designed for MERRA at various spatial resolutions
c     So instruction specifies resolution (for file name) as well as nlat/nlon
c
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
      integer dylast,hrlast,hours
      integer ny,nm,nd,nh,hint,yr1,yr2,mm1,mm2,yy,yd,dlen
      integer i,j,k,iunit,icode,inet,inet2,inet3,latv,lonv
      integer ncopn,ncvid,utopen,r,cmip 
      character*80 instfile,ofile,ohead,path,name
      character*200 ifile,ifile2,ifile3
      real*8 lat1(500),lon1(500)
      


c----------------------------------------------------------------------
c     Variable assignments.
c----------------------------------------------------------------------

      instfile = 'ingausmake'
      ofile    = 'tape11'
      
      write(6,'(/a/)')'PROGRAM: gausmake'
      call get_command_argument(1,spec)
      open(2,file=ofile,form='unformatted') 
      open(3,file=instfile,status='old')
      

c------------------------------------------------------------
c      Read instruction file.
c------------------------------------------------------------
      read(3,10)vtype,rtype,vunits,gtype,
     & dystrt,hrstrt,dystop,hrstop,hint,ilon,jlat,cmip,r
      close(3)
10    format (//a8/a5/a8/a10/i8/i2/i8/i2/i2/i3/i3/i3/i1)

c     since ALWAYS res 50
      vname='slp0'
      xname='lon0'
      yname='lat0'

c  Assumes a file of format 'name',YYYYMM,'_regrid.nc'. 
      write(name,'(a,i1.1,a)')'WRF_slp_R',r,'_'

      if (cmip.eq.1) then
        path = '/srv/ccrc/data34/z3478332/WRF/MIROC/'
      else if (cmip.eq.2) then
        path = '/srv/ccrc/data34/z3478332/WRF/ECHAM5/'
      else if (cmip.eq.3) then
        path = '/srv/ccrc/data34/z3478332/WRF/CCCMA/'
      else if (cmip.eq.4) then
        path = '/srv/ccrc/data34/z3478332/WRF/CSIROMK3/'
      else
         print *, 'Only four CMIP3 models'
         stop
      endif  

     


c------------------------------------------------------------

c------------------------------------------------------------
c     Convert start & end date to list of dates
c------------------------------------------------------------
cc Convert into yr1, mm1 & yy2, mm2
      yr1= floor(real(dystrt)/10000.0)
      mm1= mod(floor(real(dystrt)/100.0),100)
      yr2= floor(real(dystop)/10000.0)
      mm2= mod(floor(real(dystop)/100.0),100)
           
cc Open the ncep files for yr0,yr1,yr2
cc Do do-loop on months, within which you check # days
cc Then do loop on days,hours to save ausmap
cc Call the first year's netcdf file. Extract lat & lon      

      write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &,trim(name),yr1,mm1,'_regrid.nc'
      inet=ncopn(ifile,0,icode)   
      latv=ncvid(inet,yname,icode)
      call ncvgt(inet,latv,1,jlat,lat1,icode)
      latitude=real(lat1)
      lonv=ncvid(inet,xname,icode)
      call ncvgt(inet,lonv,1,ilon,lon1,icode) 
      longitude=real(lon1)

      yd=yr2-yr1
cc loop on months/day/hours to extract & write code       
      if (yd.eq.0) then   
         do i=mm1,mm2
            write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &      ,trim(name),yr1,i,'_regrid.nc'
            inet=ncopn(ifile,0,icode)   
            call getdays(yr1,i,dlen)
            if (cmip.eq.3.and.i.eq.2) then
              dlen=28
            endif
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr1,i,j,nh,inet,2)
              enddo 
            enddo
         enddo  
         
      else if (yd.eq.1) then 
         do i=mm1,12
            write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &      ,trim(name),yr1,i,'_regrid.nc'
            inet=ncopn(ifile,0,icode)   
            call getdays(yr1,i,dlen)
            if (cmip.eq.3.and.i.eq.2) then
              dlen=28
            endif
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yr1,i,j,nh,inet,2)
              enddo 
            enddo
         enddo         
         do i=1,mm2
            write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &      ,trim(name),yr2,i,'_regrid.nc'
            inet=ncopn(ifile,0,icode)   
            call getdays(yr2,i,dlen)
            if (cmip.eq.3.and.i.eq.2) then
              dlen=28
            endif
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
         write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &   ,trim(name),yr1,12,'_regrid.nc'
         inet=ncopn(ifile,0,icode)    
         do j=1,31
            do k=1,hint
              nh=(24/hint)*(k-1)
              call write_ausm(yr1,12,j,nh,inet,2)
            enddo 
         enddo
         do i=1,12
            write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &      ,trim(name),yy,i,'_regrid.nc'
            inet=ncopn(ifile,0,icode)   
            call getdays(yy,i,dlen)
            if (cmip.eq.3.and.i.eq.2) then
              dlen=28
            endif
            do j=1,dlen
              do k=1,hint
                 nh=(24/hint)*(k-1)
                 call write_ausm(yy,i,j,nh,inet,2)
              enddo 
            enddo
         enddo
         write(ifile,'(a,a,i4.4,i2.2,a)')trim(path)
     &   ,trim(name),yr2,1,'_regrid.nc'
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
      
      character*200 ifile
      character*80 header
      character*8 cdate
      character*4 ctime
      character tname
      real x(ilon,jlat)
      integer*2 work(ilon,jlat)
      integer it,ot,tt,tlen,tvar,icode,ny,nm,nd,nh
      integer inet,iunit,ivar
      integer*2 miss
      integer start(3)
      integer count(3)

CC  Identify the point of the netcdf file with the data wanted
CC  Assumes the netcdf file is of a single YEAR with 6-hourly data      
CC  Extract and unpack the needed timestep of data      
      
      call xhour(ny,nm,nd,nh,it)
      call xhour(ny,nm,1,0,ot)
      tt = (it-ot)/6+1
      
CC  Extract and unpack the needed timestep of data 
CC  This has been amended to also extract the wanted level (2=925)
      
      start(1)=1
      start(2)=1
      start(3)=tt
      count(1)=ilon
      count(2)=jlat
      count(3)=1
      
      ivar=ncvid(inet,vname,icode)
      call ncvgt(inet,ivar,start,count,x,icode)

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
      subroutine getdays(iy,im,id) !Version 3 (CCCMA) has no leap!

      integer iy,im,id  	! The input year, month & output days

      integer imonth(12)	! Used in calculating # days in this year
      integer imonthl(12)	! "					"

      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
      data imonthl/31,29,31,30,31,30,31,31,30,31,30,31/

C     Is leapyear? 

      if((mod(iy,4).eq.0).and.(mod(iy,100).ne.0))then
             id=imonthl(im)
      else if((mod(iy,400).eq.0)) then       
             id=imonthl(im)
      else
             id=imonth(im)
      end if

      return
      end


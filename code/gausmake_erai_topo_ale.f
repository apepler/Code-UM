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

      implicit none
      character*200 ifile1
      character*100 header
      character*9 ofile
      integer inet1,ncopn,ncvid,utopen,icode,latv,lonv,ivar
      integer i,j,k,ilon,jlat

      parameter (ilon=500)
      parameter (jlat=251)
      real latitude(jlat)
      real latitude2(jlat)
      real longitude(ilon)
      real x(ilon,jlat)
      real x2(ilon,jlat)
      real*8 xscale,xoff
      integer*2 work(ilon,jlat)
      integer*2 miss
      integer start(3)
      integer count(3)

      ofile    = 'ERAI_topo_75a'
      open(2,file=ofile,form='unformatted') 

      ifile1= '/srv/ccrc/data28/z3444417/Data/ERAI/2D/'//
     +       'EIN201312_an_sfc.nc'
      inet1=ncopn(ifile1,0,icode)

      latv=ncvid(inet1,'g0_lat_1',icode)
      call ncvgt(inet1,latv,1,jlat,latitude,icode)
      lonv=ncvid(inet1,'g0_lon_2',icode)
      call ncvgt(inet1,lonv,1,ilon,longitude,icode)  

      start(1)=1
      start(2)=1
      start(3)=1
      count(1)=ilon
      count(2)=jlat
      count(3)=1
      ivar=ncvid(inet1,'Z_GDS0_SFC',icode)

      call ncvgt(inet1,ivar,start,count,x,icode)
      x=x/9.80665


CC Create the header line      

      write(header,'(A8,22x,A5,5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,A10)')
     $"Z","ERAI",1980,1,1,0,"m","1.5X1.5"

CC Write to the file that's currently open    
      latitude2=latitude(jlat:1:-1)
      x2=x(:,jlat:1:-1)
      write(2)jlat
      write(2)(latitude2(j),j=1,jlat)
      write(2)ilon
      write(2)(longitude(i),i=1,ilon)
      write(2)header
      write(2)((x2(i,j),i=1,ilon),j=1,jlat)
      
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

      z = y/9.80665

      return
      end




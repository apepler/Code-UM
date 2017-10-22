      module mod_inc
         integer ilon,jlat,alon,alat
         parameter (alon=500)
         parameter (alat=500)
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
      character*200 ifile1
      character*20 ofile
      integer inet1,ncopn,ncvid,utopen,icode,latv,lonv,ivar
      character*10 sres,res

      call get_command_argument(1,sres)
      print *, sres

      if (sres.eq.'50') then
        vname='hgt0'
        xname='lon0'
        yname='lat0'
        jlat=101
        ilon=151
        ofile='WRF_topo_0'
        res='0.5X0.5'
      else if (sres.eq.'100') then
        vname='hgtH'
        xname='lonH'
        yname='latH'
        jlat=51
        ilon=76
        ofile='WRF_topo_H'
        res='1.0X1.0'
      else if (sres.eq.'150') then
        vname='hgt1'
        xname='lon1'
        yname='lat1'
        jlat=34
        ilon=51
        ofile='WRF_topo_1'
        res='1.5X1.5'
      else if (sres.eq.'250') then
        vname='hgt2'
        xname='lon2'
        yname='lat2'
        jlat=21
        ilon=31
        ofile='WRF_topo_2'
        res='2.5X2.5'
      else
         print *, 'Only applicably resolutions are 50, 100, 150 or 250'
         stop
      endif   

      open(2,file=ofile,form='unformatted') 

      ifile1= '/srv/ccrc/data34/z3478332/WRF/'//
     +        'WRF_topo_regrid.nc'
      inet1=ncopn(ifile1,0,icode)

      call write_ausm(res,inet1,2)
      end program
      
cc Returning to subroutine so i can specify x properly
      subroutine write_ausm(res,inet1,onet)  
      use mod_inc

      real x(ilon,jlat)
      real work(ilon,jlat)
      integer start(2)
      integer count(2)
      integer inet1,onet
      real*8 lat1(alat),lon1(alon)
      real latitude(alat),longitude(alon)
      character*10 res
      character*100 header

      latv=ncvid(inet1,yname,icode)
      call ncvgt(inet1,latv,1,jlat,lat1,icode)
      latitude=real(lat1)
      lonv=ncvid(inet1,xname,icode)
      call ncvgt(inet1,lonv,1,ilon,lon1,icode)
      longitude=real(lon1)

      start(1)=1
      start(2)=1
      count(1)=ilon
      count(2)=jlat
      ivar=ncvid(inet1,vname,icode)
      call ncvgt(inet1,ivar,start,count,work,icode)

CC Create the header line      

      write(header,'(A8,22x,A5,5x,I4.4,I2.2,I2.2,1x,I4.4,4x,A8,5x,A10)')
     $"Z","MERRA",1980,1,1,0,"m",res

CC Write to the file that's currently open    
      write(onet)jlat
      write(onet)(latitude(j),j=1,jlat)
      write(onet)ilon
      write(onet)(longitude(i),i=1,ilon)
      write(onet)header
      write(onet)((work(i,j),i=1,ilon),j=1,jlat)
      
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




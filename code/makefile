#----------------------------------------------------------------------
#
#     FILE: Makefile
#
#     DESCRIPTION:
#     
#     A makefile for the gausmake routine that converts
#     data of a netcdf file into a monthfile of ausmap type
#
#     HISTORY:
#
#     WRITTEN:: A. Pepler
#     LAST CHANGE: 23/08/2013
#
#----------------------------------------------------------------------

FFLAGS = -C -frecord-marker=4 #-fbounds-check #-finit-local-zero #-fno-range-check
F90C = gfortran

# Libraries
LIBS= -lnetcdff -lnetcdf -lgfortran
LIBDIRGFORT := $(shell echo $(LIBDIRGFORT))
LIBDIRNETCDF := $(shell echo $(LIBDIRNETCDF))


all: gausmake_erai gausmake_erai_spec gausmake_erai_topo gausmake_merra gausmake_merraN gausmake_wrf gausmake_ncep1 gausmake_cmip gausmake_20cr gausmake_access gausmake_erai_raijin gausmake_poama daily cmip5 gph

gausmake_erai:
	$(F90C) $(FFLAGS) gausmake_erai_kk.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai
	$(F90C) $(FFLAGS) gausmake_erai75_kk.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai75


gausmake_erai_spec:
	$(F90C) $(FFLAGS) gausmake_erai_kk_special.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_spec
	$(F90C) $(FFLAGS) gausmake_erai75_kk_special.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai75_spec

gausmake_erai_topo:
	$(F90C) $(FFLAGS) gausmake_erai_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_topo
	$(F90C) $(FFLAGS) gausmake_erai_topo_ale.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_topo_ale

gausmake_merra:
	$(F90C) $(FFLAGS) gausmake_merra_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_merra_topo
	$(F90C) $(FFLAGS) gausmake_merra.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_merra

gausmake_merraN:
	$(F90C) $(FFLAGS) gausmake_merraN_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_merraN_topo
	$(F90C) $(FFLAGS) gausmake_merraN.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_merraN
	$(F90C) $(FFLAGS) gausmake_merraN_1day.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_merraN_1day

gausmake_wrf:
	$(F90C) $(FFLAGS) gausmake_wrf_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_topo
	$(F90C) $(FFLAGS) gausmake_wrf_topo_v2.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_topo_v2
	$(F90C) $(FFLAGS) gausmake_wrf.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf
	$(F90C) $(FFLAGS) gausmake_wrf_v2.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_v2
	$(F90C) $(FFLAGS) gausmake_cmipwrf_v2.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_cmipwrf_v2
	$(F90C) $(FFLAGS) gausmake_cmipwrf_v2a.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_cmipwrf_v2a
	$(F90C) $(FFLAGS) gausmake_wrf_mine.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_mine
	$(F90C) $(FFLAGS) gausmake_wrf_dir.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_dir
	$(F90C) $(FFLAGS) gausmake_wrf_dir2.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_dir2
	$(F90C) $(FFLAGS) gausmake_wrf_mine_v2.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_mine_v2
	$(F90C) $(FFLAGS) gausmake_wrf_mine_d02.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_mine_d02
	$(F90C) $(FFLAGS) gausmake_wrf_mine_d02_v2.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_wrf_mine_d02_v2

gausmake_ncep1:
	$(F90C) $(FFLAGS) gausmake_ncep1_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_ncep1_topo
	$(F90C) $(FFLAGS) gausmake_ncep1.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_ncep1

gausmake_cmip:
	$(F90C) $(FFLAGS) gausmake_echam5.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_echam5
	$(F90C) $(FFLAGS) gausmake_cccma.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_cccma
	$(F90C) $(FFLAGS) gausmake_cmip.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_cmip

gausmake_20cr:
	$(F90C) $(FFLAGS) gausmake_20cr_ensemble.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_20cr_ensemble
	$(F90C) $(FFLAGS) gausmake_20cr_mean.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_20cr_mean
	$(F90C) $(FFLAGS) gausmake_20cr_ensemble_v2c.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_20cr_ensemble_v2c
	$(F90C) $(FFLAGS) gausmake_20cr_mean_v2c.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_20cr_mean_v2c
	$(F90C) $(FFLAGS) gausmake_20cr_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_20cr_topo

gausmake_access:
	$(F90C) $(FFLAGS) gausmake_access.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_access
	$(F90C) $(FFLAGS) gausmake_access_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_access_topo
	$(F90C) $(FFLAGS) gausmake_access_double.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_access_double

gausmake_raijin:
	$(F90C) $(FFLAGS) gausmake_erai_raijin.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_raijin
	$(F90C) $(FFLAGS) gausmake_erai_topo_raijin.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_topo_raijin

daily:
	$(F90C) $(FFLAGS) gausmake_erai_daily.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_daily

gausmake_poama:
	$(F90C) $(FFLAGS) gausmake_poama.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_poama
	$(F90C) $(FFLAGS) gausmake_poama_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_poama_topo

cmip5:
	$(F90C) $(FFLAGS) gausmake_cmip5_topo.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_cmip5_topo
	$(F90C) $(FFLAGS) gausmake_ACCESS1-3.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_ACCESS1-3

gph:
	$(F90C) $(FFLAGS) gausmake_erai_raijin_gph.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_raijin_gph
	$(F90C) $(FFLAGS) gausmake_erai_raijin_gph_daily.f $(LIBDIGFORT) $(LIBDIRNETCDF) $(LIBS) -o gausmake_erai_raijin_gph_daily

clean:
	rm -f gausmake_erai
	rm -f gausmake_erai_spec
	rm -f gausmake_erai_topo
	rm -f gausmake_erai75
	rm -f gausmake_erai75_spec
	rm -f gausmake_erai_topo_ale
	rm -f gausmake_merra
	rm -f gausmake_merra_topo
	rm -f gausmake_merraN
	rm -f gausmake_merraN_topo
	rm -f gausmake_wrf
	rm -f gausmake_wrf_v2
	rm -f gausmake_cmip
	rm -f gausmake_cmipwrf_v2
	rm -f gausmake_cmipwrf_v2a
	rm -f gausmake_wrf_topo
	rm -f gausmake_wrf_topo_v2
	rm -f gausmake_ncep1
	rm -f gausmake_ncep1_topo
	rm -f gausmake_echam5
	rm -f gausmake_cccma
	rm -f gausmake_wrf_mine
	rm -f gausmake_wrf_dir
	rm -f gausmake_wrf_dir2
	rm -f gausmake_wrf_mine_v2
	rm -f gausmake_wrf_mine_d02
	rm -f gausmake_wrf_mine_d02_v2
	rm -f gausmake_20cr_ensemble
	rm -f gausmake_20cr_mean
	rm -f gausmake_20cr_ensemble_v2c
	rm -f gausmake_20cr_mean_v2c
	rm -f gausmake_access
	rm -f gausmake_erai_raijin
	rm -f gausmake_erai_topo_raijin
	rm -f gausmake_erai_daily
	rm -f gausmake_cmip5
	rm -f gausmake_ACCESS1-3
	rm -f gausmake_erai_raijin_gph
	rm -f gausmake_erai_raijin_gph_daily

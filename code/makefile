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

FFLAGS = -C -frecord-marker=4 -finit-local-zero #-fno-range-check
F90C = gfortran

# Libraries
LIB3=-lgfortran
LIB4=-lnetcdff

# Library directories
#MISC_LIB = -L/usr/lib/gcc/x86_64-redhat-linux/4.1.2/libgfortran.a
MISC_LIB = -L/share/apps/gcc/4.8.1/lib64/libgfortran.a

all: gausmake_erai gausmake_erai_spec gausmake_erai_topo gausmake_merra gausmake_merraN gausmake_wrf gausmake_ncep1 gausmake_cmip gausmake_20cr

gausmake_erai:
	$(F90C) $(FFLAGS) gausmake_erai_kk.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_erai
	$(F90C) $(FFLAGS) gausmake_erai75_kk.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_erai75


gausmake_erai_spec:
	$(F90C) $(FFLAGS) gausmake_erai_kk_special.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_erai_spec
	$(F90C) $(FFLAGS) gausmake_erai75_kk_special.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_erai75_spec

gausmake_erai_topo:
	$(F90C) $(FFLAGS) gausmake_erai_topo.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_erai_topo
	$(F90C) $(FFLAGS) gausmake_erai_topo_ale.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_erai_topo_ale

gausmake_merra:
	$(F90C) $(FFLAGS) gausmake_merra_topo.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_merra_topo
	$(F90C) $(FFLAGS) gausmake_merra.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_merra

gausmake_merraN:
	$(F90C) $(FFLAGS) gausmake_merraN_topo.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_merraN_topo
	$(F90C) $(FFLAGS) gausmake_merraN.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_merraN
	$(F90C) $(FFLAGS) gausmake_merraN_1day.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_merraN_1day

gausmake_wrf:
	$(F90C) $(FFLAGS) gausmake_wrf_topo.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_topo
	$(F90C) $(FFLAGS) gausmake_wrf_topo_v2.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_topo_v2
	$(F90C) $(FFLAGS) gausmake_wrf.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf
	$(F90C) $(FFLAGS) gausmake_wrf_v2.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_v2
	$(F90C) $(FFLAGS) gausmake_cmipwrf_v2.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_cmipwrf_v2
	$(F90C) $(FFLAGS) gausmake_cmipwrf_v2a.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_cmipwrf_v2a
	$(F90C) $(FFLAGS) gausmake_wrf_mine.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_mine
	$(F90C) $(FFLAGS) gausmake_wrf_dir.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_dir
	$(F90C) $(FFLAGS) gausmake_wrf_dir2.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_dir2
	$(F90C) $(FFLAGS) gausmake_wrf_mine_v2.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_mine_v2
	$(F90C) $(FFLAGS) gausmake_wrf_mine_d02.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_mine_d02
	$(F90C) $(FFLAGS) gausmake_wrf_mine_d02_v2.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_wrf_mine_d02_v2

gausmake_ncep1:
	$(F90C) $(FFLAGS) gausmake_ncep1_topo.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_ncep1_topo
	$(F90C) $(FFLAGS) gausmake_ncep1.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_ncep1

gausmake_cmip:
	$(F90C) $(FFLAGS) gausmake_echam5.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_echam5
	$(F90C) $(FFLAGS) gausmake_cccma.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_cccma
	$(F90C) $(FFLAGS) gausmake_cmip.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_cmip

gausmake_20cr:
	$(F90C) $(FFLAGS) gausmake_20cr_ensemble.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_20cr_ensemble
	$(F90C) $(FFLAGS) gausmake_20cr_mean.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_20cr_mean
	$(F90C) $(FFLAGS) gausmake_20cr_ensemble_v2c.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_20cr_ensemble_v2c
	$(F90C) $(FFLAGS) gausmake_20cr_mean_v2c.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_20cr_mean_v2c
	$(F90C) $(FFLAGS) gausmake_20cr_topo.f $(MISC_LIB) $(LIB3) $(LIB4) -o gausmake_20cr_topo

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

# -*-Makefile-*-
#

include ../../setting/macros.make

vpath %.F90 src

CPPFLAGS = -DOGCM_TRIPOLAR -DOGCM_CYCLIC -DOGCM_VARIABLE -DOGCM_FREESURFACE -DOGCM_BBL

#
#     SUFFIXES rule
#
### Compile ###
%.o: %.F90
	$(F90) -c $(FFLAGS) $(INCLUDES) $(CPPFLAGS) -o $@ $<


### Link ###
%: %.o $(lib)
	$(F90) $^ $(LDFLAGS) -o $@

### clean ###

clean:
	rm *.o core $(targets) *.mod *.a *~ 2> /dev/null

############

param.o : param.F90
stateeq.o : stateeq.F90
common.o : common.F90 param.o
trnsfrm.o : trnsfrm.F90 param.o
dens.o : dens.F90 param.o
density.o : density.F90 param.o
strhgt.o : strhgt.F90 param.o common.o dens.o
ts2steric.o : ts2steric.F90 param.o common.o density.o
hs2hm.o : hs2hm.F90 param.o common.o
hs2hm_basin.o : hs2hm_basin.F90 param.o common.o
hm_basin_2d.o : hm_basin_2d.F90 param.o common.o
icearea.o : icearea.F90 param.o common.o
iceclim.o : iceclim.F90 param.o common.o
iceacorr.o : iceacorr.F90 param.o common.o dens.o
mkgrhist_tsclim.o : mkgrhist_tsclim.F90 param.o common.o
mkgrhist_0101.o : mkgrhist_0101.F90 param.o common.o
rivd_smooth.o : rivd_smooth.F90 param.o common.o
rivd_smooth_u.o : rivd_smooth_u.F90 param.o common.o
rivd_southern.o : rivd_southern.F90 param.o common.o
make_montcl_t.o : make_moncl_t.F90 param.o common.o
merid_ht_ann.o : merid_ht_ann.F90 param.o common.o dens.o
merid_ht_pac_ann.o : merid_ht_pac_ann.F90 param.o common.o dens.o
merid_ht_pac_ann_geo.o : merid_ht_pac_ann_geo.F90 param.o common.o trnsfrm.o
merid_wt_ann.o : merid_wt_ann.F90 param.o common.o dens.o
merid_sal_ann.o : merid_sal_ann.F90 param.o common.o dens.o
isotherm_dep.o : isotherm_dep.F90 param.o common.o
surf_heat_all_ann.o : surf_heat_all_ann.F90 param.o common.o trnsfrm.o
surf_heat_component_ann.o : surf_heat_component_ann.F90 param.o common.o trnsfrm.o
surf_heat_diag.o : surf_heat_diag.F90 param.o common.o trnsfrm.o
surf_water_diag.o : surf_water_diag.F90 param.o common.o trnsfrm.o
surf_water_all_ann.o : surf_water_all_ann.F90 param.o common.o trnsfrm.o
surf_water_component_ann.o : surf_water_component_ann.F90 param.o common.o trnsfrm.o
icesurf_water_component_ann.o : icesurf_water_component_ann.F90 param.o common.o trnsfrm.o
surf_buoyancy_flux_ann.o : surf_buoyancy_flux_ann.F90 param.o common.o stateeq.o
surf_effective_heat_flux_mon.o : surf_effective_heat_flux_mon.F90 param.o common.o stateeq.o
saltflux2waterflux.o : saltflux2waterflux.F90 param.o common.o
mk_baro_america.o : mk_baro_america.F90 param.o common.o
mkvat_ann.o : mkvat_ann.F90 param.o common.o

mkgrhist_interior_t.o : mkgrhist_interior_t.F90 param.o common.o
nc2grads_march_mld.o: nc2grads_march_mld.F90

dummy_ssh.o : dummy_ssh.F90 param.o common.o

strhgt : strhgt.o param.o common.o dens.o
	$(F90) -o $@ strhgt.o param.o common.o dens.o $(FFLAGS)  

ts2steric : ts2steric.o param.o common.o density.o
	$(F90) -o $@ ts2steric.o param.o common.o density.o $(FFLAGS)  

merid_ht : merid_ht.o param.o common.o dens.o
	$(F90) -o $@ merid_ht.o param.o common.o dens.o $(FFLAGS)  

merid_ht_ann : merid_ht_ann.o param.o common.o dens.o
	$(F90) -o $@ merid_ht_ann.o param.o common.o dens.o $(FFLAGS)  

merid_ht_pac_ann : merid_ht_pac_ann.o param.o common.o dens.o
	$(F90) -o $@ merid_ht_pac_ann.o param.o common.o dens.o $(FFLAGS)  

merid_ht_pac_ann_geo : merid_ht_pac_ann_geo.o param.o common.o trnsfrm.o
	$(F90) -o $@ merid_ht_pac_ann_geo.o param.o common.o trnsfrm.o $(FFLAGS)  

merid_wt_ann : merid_wt_ann.o param.o common.o dens.o
	$(F90) -o $@ merid_wt_ann.o param.o common.o denso $(FFLAGS)  

merid_sal_ann : merid_sal_ann.o param.o common.o dens.o
	$(F90) -o $@ merid_sal_ann.o param.o common.o dens.o $(FFLAGS)  

surf_heat_all_ann : surf_heat_all_ann.o param.o common.o trnsfrm.o
	$(F90) -o $@ surf_heat_all_ann.o param.o common.o trnsfrm.o $(FFLAGS)  

surf_heat_diag : surf_heat_diag.o param.o common.o trnsfrm.o
	$(F90) -o $@ surf_heat_diag.o param.o common.o trnsfrm.o $(FFLAGS)  

surf_water_diag : surf_water_diag.o param.o common.o trnsfrm.o
	$(F90) -o $@ surf_water_diag.o param.o common.o trnsfrm.o $(FFLAGS)  

surf_heat_component_ann : surf_heat_component_ann.o param.o common.o trnsfrm.o
	$(F90) -o $@ surf_heat_component_ann.o param.o common.o trnsfrm.o $(FFLAGS)  

surf_water_all_ann : surf_water_all_ann.o param.o common.o trnsfrm.o
	$(F90) -o $@ surf_water_all_ann.o param.o common.o trnsfrm.o $(FFLAGS)  

surf_water_component_ann : surf_water_component_ann.o param.o common.o trnsfrm.o
	$(F90) -o $@ surf_water_component_ann.o param.o common.o trnsfrm.o $(FFLAGS)  

icesurf_water_component_ann : icesurf_water_component_ann.o param.o common.o trnsfrm.o
	$(F90) -o $@ icesurf_water_component_ann.o param.o common.o trnsfrm.o $(FFLAGS)  

surf_buoyancy_flux_ann : surf_buoyancy_flux_ann.o param.o common.o stateeq.o
	$(F90) -o $@ surf_buoyancy_flux_ann.o param.o common.o stateeq.o $(FFLAGS)  

surf_effective_heat_flux_mon : surf_effective_heat_flux_mon.o param.o common.o stateeq.o
	$(F90) -o $@ surf_effective_heat_flux_mon.o param.o common.o stateeq.o $(FFLAGS)  

saltflux2waterflux : saltflux2waterflux.o param.o common.o
	$(F90) -o $@ saltflux2waterflux.o param.o common.o $(FFLAGS)  

mk_baro_america : mk_baro_america.o param.o common.o
	$(F90) -o $@ mk_baro_america.o param.o common.o $(FFLAGS)  

mkvat_ann : mkvat_ann.o param.o common.o
	$(F90) -o $@ mkvat_ann.o param.o common.o $(FFLAGS)  

isotherm_dep : isotherm_dep.o param.o common.o
	$(F90) -o $@ isotherm_dep.o param.o common.o $(FFLAGS)  

hs2hm : hs2hm.o param.o common.o
	$(F90) -o $@ hs2hm.o param.o common.o $(FFLAGS)  

hs2hm_basin : hs2hm_basin.o param.o common.o
	$(F90) -o $@ hs2hm_basin.o param.o common.o $(FFLAGS)  

hm_basin_2d : hm_basin_2d.o param.o common.o
	$(F90) -o $@ hm_basin_2d.o param.o common.o $(FFLAGS)  

mkgrhist_tsclim : mkgrhist_tsclim.o param.o common.o
	$(F90) -o $@ mkgrhist_tsclim.o param.o common.o $(FFLAGS)  

mkgrhist_interior_t : mkgrhist_interior_t.o param.o common.o
	$(F90) -o $@ mkgrhist_interior_t.o param.o common.o $(FFLAGS)  

mkgrhist_0101 : mkgrhist_0101.o param.o common.o
	$(F90) -o $@ mkgrhist_0101.o param.o common.o $(FFLAGS)  

rivd_smooth : rivd_smooth.o param.o common.o
	$(F90) -o $@ rivd_smooth.o param.o common.o $(FFLAGS)  

rivd_smooth_u : rivd_smooth_u.o param.o common.o
	$(F90) -o $@ rivd_smooth_u.o param.o common.o $(FFLAGS)  

rivd_southern : rivd_southern.o param.o common.o
	$(F90) -o $@ rivd_southern.o param.o common.o $(FFLAGS)  

make_moncl_t : make_moncl_t.o param.o common.o
	$(F90) -o $@ make_moncl_t.o param.o common.o $(FFLAGS)  

dummy_ssh : dummy_ssh.o param.o common.o
	$(F90) -o $@ dummy_ssh.o param.o common.o $(FFLAGS)  

icearea : icearea.o param.o common.o
	$(F90) -o $@ icearea.o param.o common.o $(FFLAGS) 

iceclim : iceclim.o param.o common.o
	$(F90) -o $@ iceclim.o param.o common.o $(FFLAGS) 

iceacorr : iceacorr.o param.o common.o dens.o
	$(F90) -o $@ iceacorr.o param.o common.o dens.o $(FFLAGS) 

nc2grads_march_mld : nc2grads_march_mld.o
	$(F90) -o $@ nc2grads_march_mld.o $(FFLAGS) $(LINKOPT) 

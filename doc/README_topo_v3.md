Grid Mask
========

   * Mapping onto the regular lat-lon grid
     - extract_jra55/rgrid2latlon_mask_exec.sh

   * Bury lake and pond
     - jra55_topo/divide_to_text.sh and unite_to_binary.sh

   * Create mapping table onto COBESST grid
     - jra55_on_cobesst/tmp10m_jra55toCOBE_snap_all.sh
     - jra55_on_cobesst/tmp10m_jra55toCOBE_snap_ocean.sh

   * See cobesst directory for details of creating mask of COBESST data set

   * Unite with COBESST mask
     - jra55_on_cobesst/unite_mask_jra_cobe_exec.sh

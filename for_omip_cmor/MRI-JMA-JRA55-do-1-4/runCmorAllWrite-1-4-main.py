#!/usr/bin/env python2.7
import cmor,json,os
import cdms2 as cdm
import cdtime as cdt
import numpy as np
from calendar import isleap
#cdm.setAutoBounds('on') # Caution, this attempts to automatically set coordinate bounds - please check outputs using this option
#import pdb ; # Debug statement - import if enabling debugging below

# Notes
# PJD  5 Feb 2018   - Started
# PJD 21 Feb 2018   - Updated for CMOR3.3.1
# PJD 24 Feb 2018   - Updated paths for demo dir
# PJD  7 Mar 2018   - Updated pr to prra
# PJD  8 Mar 2018   - Correct prra 'positive'; Added ficeberg2d placeholder
# PJD  5 Apr 2018   - Update for latest variable list
#                   TODO: Fix missing_value assignment problem
#                     ---> Is this going to be fixed by setting missing_value to 1e20 for the inputfile?
#                          This has been done for JRA55-do-v1.3.1

#%% Create input decks for all variables - 'fileList' will need to be amended to include all files 1958-2018
inputDict = {}
inputDict['LIday'] = {}
key = 'licalvf'
inputDict['LIday'][key] = {}
inputDict['LIday'][key]['fileList'] = [
        'input_atmos/runoff_solidice.1958.07Jan2019.nc',
        'input_atmos/runoff_solidice.1959.07Jan2019.nc',
        'input_atmos/runoff_solidice.1960.07Jan2019.nc',
        'input_atmos/runoff_solidice.1961.07Jan2019.nc',
        'input_atmos/runoff_solidice.1962.07Jan2019.nc',
        'input_atmos/runoff_solidice.1963.07Jan2019.nc',
        'input_atmos/runoff_solidice.1964.07Jan2019.nc',
        'input_atmos/runoff_solidice.1965.07Jan2019.nc',
        'input_atmos/runoff_solidice.1966.07Jan2019.nc',
        'input_atmos/runoff_solidice.1967.07Jan2019.nc',
        'input_atmos/runoff_solidice.1968.07Jan2019.nc',
        'input_atmos/runoff_solidice.1969.07Jan2019.nc',
        'input_atmos/runoff_solidice.1970.07Jan2019.nc',
        'input_atmos/runoff_solidice.1971.07Jan2019.nc',
        'input_atmos/runoff_solidice.1972.07Jan2019.nc',
        'input_atmos/runoff_solidice.1973.07Jan2019.nc',
        'input_atmos/runoff_solidice.1974.07Jan2019.nc',
        'input_atmos/runoff_solidice.1975.07Jan2019.nc',
        'input_atmos/runoff_solidice.1976.07Jan2019.nc',
        'input_atmos/runoff_solidice.1977.07Jan2019.nc',
        'input_atmos/runoff_solidice.1978.07Jan2019.nc',
        'input_atmos/runoff_solidice.1979.07Jan2019.nc',
        'input_atmos/runoff_solidice.1980.07Jan2019.nc',
        'input_atmos/runoff_solidice.1981.07Jan2019.nc',
        'input_atmos/runoff_solidice.1982.07Jan2019.nc',
        'input_atmos/runoff_solidice.1983.07Jan2019.nc',
        'input_atmos/runoff_solidice.1984.07Jan2019.nc',
        'input_atmos/runoff_solidice.1985.07Jan2019.nc',
        'input_atmos/runoff_solidice.1986.07Jan2019.nc',
        'input_atmos/runoff_solidice.1987.07Jan2019.nc',
        'input_atmos/runoff_solidice.1988.07Jan2019.nc',
        'input_atmos/runoff_solidice.1989.07Jan2019.nc',
        'input_atmos/runoff_solidice.1990.07Jan2019.nc',
        'input_atmos/runoff_solidice.1991.07Jan2019.nc',
        'input_atmos/runoff_solidice.1992.07Jan2019.nc',
        'input_atmos/runoff_solidice.1993.07Jan2019.nc',
        'input_atmos/runoff_solidice.1994.07Jan2019.nc',
        'input_atmos/runoff_solidice.1995.07Jan2019.nc',
        'input_atmos/runoff_solidice.1996.07Jan2019.nc',
        'input_atmos/runoff_solidice.1997.07Jan2019.nc',
        'input_atmos/runoff_solidice.1998.07Jan2019.nc',
        'input_atmos/runoff_solidice.1999.07Jan2019.nc',
        'input_atmos/runoff_solidice.2000.07Jan2019.nc',
        'input_atmos/runoff_solidice.2001.07Jan2019.nc',
        'input_atmos/runoff_solidice.2002.07Jan2019.nc',
        'input_atmos/runoff_solidice.2003.07Jan2019.nc',
        'input_atmos/runoff_solidice.2004.07Jan2019.nc',
        'input_atmos/runoff_solidice.2005.07Jan2019.nc',
        'input_atmos/runoff_solidice.2006.07Jan2019.nc',
        'input_atmos/runoff_solidice.2007.07Jan2019.nc',
        'input_atmos/runoff_solidice.2008.07Jan2019.nc',
        'input_atmos/runoff_solidice.2009.07Jan2019.nc',
        'input_atmos/runoff_solidice.2010.07Jan2019.nc',
        'input_atmos/runoff_solidice.2011.07Jan2019.nc',
        'input_atmos/runoff_solidice.2012.07Jan2019.nc',
        'input_atmos/runoff_solidice.2013.07Jan2019.nc',
        'input_atmos/runoff_solidice.2014.07Jan2019.nc',
        'input_atmos/runoff_solidice.2015.07Jan2019.nc',
        'input_atmos/runoff_solidice.2016.07Jan2019.nc',
        'input_atmos/runoff_solidice.2017.07Jan2019.nc',
        ]
inputDict['LIday'][key]['inputVarName'] = 'licalvf'
inputDict['LIday'][key]['outputVarName'] = 'licalvf'
inputDict['LIday'][key]['outputUnits'] = 'kg m-2 s-1'
inputDict['LIday'][key]['positive'] = ''
inputDict['Oday'] = {}
#key = 'ficeberg2d'
#inputDict['Oday'][key] = {}
#inputDict['Oday'][key]['fileList'] = [
#        ''
#        ]
#inputDict['Oday'][key]['inputVarName'] = 'ficeberg'
#inputDict['Oday'][key]['outputVarName'] = 'ficeberg'
#inputDict['Oday'][key]['outputUnits'] = 'kg m-2 s-1'
#inputDict['Oday'][key]['positive'] = ''
key = 'runoff_all'
inputDict['Oday'][key] = {}
inputDict['Oday'][key]['fileList'] = [
        'input_atmos/runoff_all.1958.07Jan2019.nc',
        'input_atmos/runoff_all.1959.07Jan2019.nc',
        'input_atmos/runoff_all.1960.07Jan2019.nc',
        'input_atmos/runoff_all.1961.07Jan2019.nc',
        'input_atmos/runoff_all.1962.07Jan2019.nc',
        'input_atmos/runoff_all.1963.07Jan2019.nc',
        'input_atmos/runoff_all.1964.07Jan2019.nc',
        'input_atmos/runoff_all.1965.07Jan2019.nc',
        'input_atmos/runoff_all.1966.07Jan2019.nc',
        'input_atmos/runoff_all.1967.07Jan2019.nc',
        'input_atmos/runoff_all.1968.07Jan2019.nc',
        'input_atmos/runoff_all.1969.07Jan2019.nc',
        'input_atmos/runoff_all.1970.07Jan2019.nc',
        'input_atmos/runoff_all.1971.07Jan2019.nc',
        'input_atmos/runoff_all.1972.07Jan2019.nc',
        'input_atmos/runoff_all.1973.07Jan2019.nc',
        'input_atmos/runoff_all.1974.07Jan2019.nc',
        'input_atmos/runoff_all.1975.07Jan2019.nc',
        'input_atmos/runoff_all.1976.07Jan2019.nc',
        'input_atmos/runoff_all.1977.07Jan2019.nc',
        'input_atmos/runoff_all.1978.07Jan2019.nc',
        'input_atmos/runoff_all.1979.07Jan2019.nc',
        'input_atmos/runoff_all.1980.07Jan2019.nc',
        'input_atmos/runoff_all.1981.07Jan2019.nc',
        'input_atmos/runoff_all.1982.07Jan2019.nc',
        'input_atmos/runoff_all.1983.07Jan2019.nc',
        'input_atmos/runoff_all.1984.07Jan2019.nc',
        'input_atmos/runoff_all.1985.07Jan2019.nc',
        'input_atmos/runoff_all.1986.07Jan2019.nc',
        'input_atmos/runoff_all.1987.07Jan2019.nc',
        'input_atmos/runoff_all.1988.07Jan2019.nc',
        'input_atmos/runoff_all.1989.07Jan2019.nc',
        'input_atmos/runoff_all.1990.07Jan2019.nc',
        'input_atmos/runoff_all.1991.07Jan2019.nc',
        'input_atmos/runoff_all.1992.07Jan2019.nc',
        'input_atmos/runoff_all.1993.07Jan2019.nc',
        'input_atmos/runoff_all.1994.07Jan2019.nc',
        'input_atmos/runoff_all.1995.07Jan2019.nc',
        'input_atmos/runoff_all.1996.07Jan2019.nc',
        'input_atmos/runoff_all.1997.07Jan2019.nc',
        'input_atmos/runoff_all.1998.07Jan2019.nc',
        'input_atmos/runoff_all.1999.07Jan2019.nc',
        'input_atmos/runoff_all.2000.07Jan2019.nc',
        'input_atmos/runoff_all.2001.07Jan2019.nc',
        'input_atmos/runoff_all.2002.07Jan2019.nc',
        'input_atmos/runoff_all.2003.07Jan2019.nc',
        'input_atmos/runoff_all.2004.07Jan2019.nc',
        'input_atmos/runoff_all.2005.07Jan2019.nc',
        'input_atmos/runoff_all.2006.07Jan2019.nc',
        'input_atmos/runoff_all.2007.07Jan2019.nc',
        'input_atmos/runoff_all.2008.07Jan2019.nc',
        'input_atmos/runoff_all.2009.07Jan2019.nc',
        'input_atmos/runoff_all.2010.07Jan2019.nc',
        'input_atmos/runoff_all.2011.07Jan2019.nc',
        'input_atmos/runoff_all.2012.07Jan2019.nc',
        'input_atmos/runoff_all.2013.07Jan2019.nc',
        'input_atmos/runoff_all.2014.07Jan2019.nc',
        'input_atmos/runoff_all.2015.07Jan2019.nc',
        'input_atmos/runoff_all.2016.07Jan2019.nc',
        'input_atmos/runoff_all.2017.07Jan2019.nc',
        ]
inputDict['Oday'][key]['inputVarName'] = 'friver'
inputDict['Oday'][key]['outputVarName'] = 'friver'
inputDict['Oday'][key]['outputUnits'] = 'kg m-2 s-1'
inputDict['Oday'][key]['positive'] = ''
# fixed in time
inputDict['LIfx'] = {}
key = 'areacellg'
inputDict['LIfx'][key] = {}
inputDict['LIfx'][key]['fileList'] = [
        'input_suppl/runoff_cell_area.10Apr2018.nc'
        ]
inputDict['LIfx'][key]['inputVarName'] = 'areacellg'
inputDict['LIfx'][key]['outputVarName'] = 'areacellg'
inputDict['LIfx'][key]['outputUnits'] = 'm2'
inputDict['LIfx'][key]['positive'] = ''

#%% Loop through entries and process file lists
for key in inputDict.keys():
    # User provided input
    cmorTable = ''.join(['Tables/input4MIPs_',key,'.json']) ; # Aday,Amon,Lmon,Omon,SImon,fx - Load target table, axis info (coordinates, grid*) and CVs
    cmorJson = json.load(open(cmorTable))
    inputJson = 'mriJRA55-do-input.json' ; # Update contents of this file to set your global_attributes
    newJson = json.load(open(inputJson))
    for var in inputDict[key].keys():
        outVar = inputDict[key][var]['outputVarName']
        # Update frequency based on variableand write output to CMOR input file
        newJson['frequency'] = cmorJson['variable_entry'][outVar]['frequency']
        json.dump(newJson,open('tmp.json','w'),ensure_ascii=True,encoding='utf-8',sort_keys=True)
        inputVarName = inputDict[key][var]['inputVarName']
        outputVarName = inputDict[key][var]['outputVarName']
        outputUnits = inputDict[key][var]['outputUnits']
        for count,filePath in enumerate(inputDict[key][var]['fileList']):
            print 'key,var:',key,var
            print 'filePath:',filePath
            print 'cmorTable:',cmorTable
            print 'inputVarName:',inputVarName
            print 'outputVarName:',outputVarName
            print 'outputUnits:',outputUnits
            # Open and read input netcdf file
            f = cdm.open(filePath)
            print 'Source data read start..'
            d = f(inputVarName) ; # Or use temporal subset for testing below
            #d = f(inputVarName,time=slice(0,5))
            print 'Source data read end..'
            # Reset missing value
            #d.setMissing(1e20) ; # This should also set fill_value, and suppress CMOR variable history being written
            # Get axes
            lat = d.getLatitude()
            lon = d.getLongitude()
            if var in ['uo','vo']:
                lev = d.getLevel()
            time = d.getTime() ; # Assumes variable is named 'time', for the demo file this is named 'months'
            #time = d.getAxis(0) ; # Rather use a file dimension-based load

            #%% Initialize and run CMOR
            print 'Start CMORizing..'
            # For more information see https://cmor.llnl.gov/mydoc_cmor3_api/
            cmor.setup(inpath='./',netcdf_file_action=cmor.CMOR_REPLACE_4) #,logfile='cmorLog.txt')
            cmor.dataset_json('tmp.json')
            cmor.load_table(cmorTable)
            #cmor.set_cur_dataset_attribute('history',f.history) ; # Force input file attribute as history
            # Create axes based on variable
            if var in ['q_10','t_10']:
                # Reset height2m coordinate value to 10m
                heightAx = {'table_entry': 'height2m',
                            'units': 'm',
                            'coord_vals': cdm.createAxis([10.],id='height')
                            }
            elif var in ['u_10','v_10']:
                # Use height10m coordinate entry
                heightAx = {'table_entry': 'height10m',
                            'units': 'm',
                            'coord_vals': cdm.createAxis([10.],id='height')
                            }
            else:
                # Use height2m default value
                heightAx = {'table_entry': 'height2m',
                            'units': 'm',
                            'coord_vals': cdm.createAxis([2.],id='height')
                            }
            # Create time based on table
            if key == 'A3hrPt':
                timeAx = {'table_entry': 'time1',
                          'units': time.units
                         }
            elif key == 'SI3hrPt':
                timeAx = {'table_entry': 'time1',
                          'units': time.units
                         }
            elif key == 'OmonC':
                # Create climatological time axis for the WOA13 (1955-2012) temporal range
                times = [] ; times_bnds = []
                for count,months in enumerate(range(1,13)):
                    #print count,months
                    if months in [1,3,5,7,8,10,12]:
                        monthendday = 31
                        hrs = 12
                    elif months in [4,6,9,11]:
                        monthendday = 30
                        hrs = 0
                    elif isleap(int(1984)):
                        monthendday = 29
                        hrs = 12
                    else:
                        monthendday = 28
                        hrs = 0
                    times.append(cdt.componenttime(1984,months,np.int(monthendday/2.),hrs).torel('days since 1955-1-1').value)
                    # WOA13v2 extends from 1955 to 2012
#                    times_bnds.append([cdt.componenttime(1955,months,1,0,0,0).torel('days since 1955-1-1'),
#                                       cdt.componenttime(2012,months,monthendday,12,59,59).torel('days since 1955-1-1')])
                    if months < 12:
                        times_bnds.append([cdt.componenttime(1955,months,1,0,0,0).torel('days since 1955-1-1').value,
                                           cdt.componenttime(2012,months+1,1,0,0,0).torel('days since 1955-1-1').value])
                    else:
                        times_bnds.append([cdt.componenttime(1955,months,1,0,0,0).torel('days since 1955-1-1').value,
                                           cdt.componenttime(2013,1,1,0,0,0).torel('days since 1955-1-1').value])
                time = cdm.createAxis(times,np.array(times_bnds),id='time')
                time.units = 'days since 1955-01-01 0.0.0'
                timeAx = {'table_entry': 'time2',
                          'units': time.units,
                          'coord_vals': time
                         }
            elif key == 'OyrC':
                if var in ['uo','vo']:
                    # Create climatological time axis for the GlobCurrent (Nov1999-Oct2009) temporal range
                    times = [] ; times_bnds = []
                    times.append(cdt.componenttime(2004,10,31,12,0,0).torel('days since 1999-11-1').value)
                    times_bnds.append([cdt.componenttime(1999,11,1,0,0,0).torel('days since 1999-11-1').value,
                                       cdt.componenttime(2009,11,1,0,0,0).torel('days since 1999-11-1').value])
                    time = cdm.createAxis(times,np.array(times_bnds),id='time')
                    time.units = 'days since 1999-11-01 0.0.0'
                    print times[0]
                    print times_bnds[0]
                    print time
                    timeAx = {'table_entry': 'time2',
                              'units': time.units,
                              'coord_vals': time
                             }
            elif key == 'LIyrC':
                if var in ['licalvf']:
                    # Create climatological time axis for the Depoorter et al. (2013) (Jan2007-Dec2007) temporal range
                    times = [] ; times_bnds = []
                    times.append(cdt.componenttime(2007,7,2,12,0,0).torel('days since 2007-1-1').value)
                    times_bnds.append([cdt.componenttime(2007,1,1,0,0,0).torel('days since 2007-1-1').value,
                                       cdt.componenttime(2008,1,1,0,0,0).torel('days since 2007-1-1').value])
                    time = cdm.createAxis(times,np.array(times_bnds),id='time')
                    time.units = 'days since 2007-01-01 0.0.0'
                    print times[0]
                    print times_bnds[0]
                    print time
                    timeAx = {'table_entry': 'time2',
                              'units': time.units,
                              'coord_vals': time
                             }
            else:
                # Use default time entry
                timeAx = {'table_entry': 'time',
                          'units': time.units
                         }
            # Now set axes list based on variable
            if var in ['q_10','t_10','u_10','v_10']:
            # 4D variable - 4 axes
                axes    = [  timeAx,
                             heightAx,
                             {'table_entry': 'latitude',
                              'units': 'degrees_north',
                              'coord_vals': lat[:],
                              'cell_bounds': lat.getBounds()
                             },
                             {'table_entry': 'longitude',
                              'units': 'degrees_east',
                              'coord_vals': lon[:],
                              'cell_bounds': lon.getBounds()
                             },
                          ]
            elif var in ['uo','vo']:
            # 4D variable - 4 axes
                axes    = [  timeAx,
                             {'table_entry': 'depth_coord',
                              'units': 'm',
                              'coord_vals': lev[:],
                              'cell_bounds': lev.getBounds()
                             },
                             {'table_entry': 'latitude',
                              'units': 'degrees_north',
                              'coord_vals': lat[:],
                              'cell_bounds': lat.getBounds()
                             },
                             {'table_entry': 'longitude',
                              'units': 'degrees_east',
                              'coord_vals': lon[:],
                              'cell_bounds': lon.getBounds()
                             },
                          ]
            elif var in ['areacello','sftof','areacellg']:
            # 2D variable - 2 axes
                axes    = [  {'table_entry': 'latitude',
                              'units': 'degrees_north',
                              'coord_vals': lat[:],
                              'cell_bounds': lat.getBounds()
                             },
                             {'table_entry': 'longitude',
                              'units': 'degrees_east',
                              'coord_vals': lon[:],
                              'cell_bounds': lon.getBounds()
                             },
                          ]
            else:
            # 3D variable - 3 axes
                axes    = [  timeAx,
                             {'table_entry': 'latitude',
                              'units': 'degrees_north',
                              'coord_vals': lat[:],
                              'cell_bounds': lat.getBounds()
                             },
                             {'table_entry': 'longitude',
                              'units': 'degrees_east',
                              'coord_vals': lon[:],
                              'cell_bounds': lon.getBounds()
                             },
                          ]
            axisIds = list() ; # Create list of axes and build these for each variable from axis components above
            for axis in axes:
                axisId = cmor.axis(**axis)
                axisIds.append(axisId)

            # For use in debugging
            #print 'axes:',axes
            #print 'd.shape:',d.shape
            #print 'outputVarName:',outputVarName
            #print 'd.units:',d.units
            #print 'd.missing:',d.missing
            #pdb.set_trace() ; # Debug statement

            # Setup units and create variable to write using cmor - see https://cmor.llnl.gov/mydoc_cmor3_api/#cmor_set_variable_attribute
            d.units = outputUnits
            if inputDict[key][var]['positive'] == 'down':
                varid   = cmor.variable(outputVarName,d.units,axisIds,missing_value=d.missing,positive='down')
            else:
                varid   = cmor.variable(outputVarName,d.units,axisIds,missing_value=d.missing) ; # This is not trapping the correct missing_value
            values  = np.array(d[:],np.float32)

            # Append valid_min and valid_max to variable before writing - see https://cmor.llnl.gov/mydoc_cmor3_api/#cmor_set_variable_attribute
            #cmor.set_variable_attribute(varid,'valid_min','f',2.0)
            #cmor.set_variable_attribute(varid,'valid_max','f',3.0)

            # Prepare variable for writing, then write and close file - see https://cmor.llnl.gov/mydoc_cmor3_api/#cmor_set_variable_attribute
            cmor.set_deflate(varid,1,1,1) ; # shuffle=1,deflate=1,deflate_level=1 - Deflate options compress file data
            print 'Start CMOR write..'
            if key == 'OmonC':
                cmor.write(varid,values) ; # Write variable with time axis
            elif key == 'OyrC':  
                cmor.write(varid,values) ; # Write variable with time axis
            elif key == 'LIyrC':  
                cmor.write(varid,values) ; # Write variable with time axis
            elif key == 'Ofx':  
                cmor.write(varid,values) ; # Write variable without time axis
            elif key == 'LIfx':  
                cmor.write(varid,values) ; # Write variable without time axis
            else:
                cmor.write(varid,values,time_vals=time,time_bnds=time.getBounds()) ; # Write variable with time axis
            print 'End CMOR write..'
            # Alteratively write in append mode
            #for i in range(0,len(time),10):
            #    print i
            #    cmor.write(varid,values[i*10:(i+1)*10],time_vals=time[i*10:(i+1)*10],time_bnds=time.getBounds()[i*10:(i+1)*10]) ; # Write variable with time axis
            f.close()
            cmor.close()
        # Cleanup
        os.remove('tmp.json')

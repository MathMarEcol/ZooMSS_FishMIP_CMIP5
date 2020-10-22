#!/usr/bin/python
import numpy as np
import xarray as xr
import glob
import pandas as pd
from datetime import datetime
from dateutil.relativedelta import relativedelta
import fnmatch
import re

### Import spatial masks for: Humboldt (1), North Sea(2), Med (3), Baltic (4), SE_Australia (5), 
### East Bass Strait (6), and we will add in the Cook Strait (7)

mask = 'fishMIP_regional_mask3_outputs.nc'
fmp_path = mask

# open fishmip mask 
cdata =  xr.open_dataset(fmp_path) # Open fishmip mask file
cmask = cdata.variables['fishMIP_MASK'][:] # Pull out mask
#cmask[47,176] = 7 # Add in the Cook Strait

## Find out how many areas there are. To do this, need to turn mask value for blank areas -
## usually some really large number - to NAN. Easier to use numpy array to do this. 
cmask_INDEX = cdata['fishMIP_MASK'][:].values # Numpy array of fishmip mask
#cmask_INDEX[47,176] = 7 # Add in the Cook Strait

if np.nanmax(cmask_INDEX) > 100000: # If max number in mask is really large...
   cmask_INDEX[cmask_INDEX == np.nanmax(cmask_INDEX)] = np.NAN # Large mask value to NA

num_areas = 7# np.nanmax(cmask_INDEX) # How many areas are there? (We know there are 9)
#num_areas = num_areas.astype(int)# Convert to integer from numpy array
name_areas = ["Humboldt", "North_Sea", "Mediterranean", "Baltic_Sea", "SE_Australia", "East_Bass_Strait", "Cook_Strait"]#, "Hawaiian", "South_Africa"] # Names of the nine areas

#### Get areas of 180x360 global map
Re = 6371
tarea = np.zeros([180,360])
lats = np.arange(-90,91)

dx = np.pi/180

for i in range(0, 180):
    min_lat = lats[i]
    max_lat = lats[i+1]

    dy = np.sin(max_lat*np.pi/180) - np.sin(min_lat*np.pi/180)
    
    tarea[i,:] = dx*dy*Re*Re
    

###########################################
###########################################
####              ZooMSS               ####
###########################################
###########################################
model = ['ZooMSS']    

roots = ['/mnt/lustre01/work/bb0820/ISIMIP/ISIMIP2b/OutputData/marine-fishery_global/ZooMSS/', '/mnt/lustre01/work/bb0820/ISIMIP/ISIMIP2a/OutputData/marine-fishery_global/ZooMSS/', '/mnt/lustre01/work/bb0820/ISIMIP/ISIMIP2b/OutputData/marine-fishery_global/ZooMSS/']
run = ['ipsl-cm5a-lr', 'gfdl-esm2m'] #run = ['ipsl-cm5a-lr','gfdl-reanalysis', 'gfdl-esm2m']
dirs = ['historical', 'future'] ## gfdl-reanalysis is historical only ### CHECK THIS WITH RYAN ONLY
fishing = ['no-fishing']

sub_dirs_h = ["hist"]
sub_dirs_f = ["rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5"] #sub_dirs_f = ["rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5"]
sub_dirs = [sub_dirs_h, sub_dirs_f]

vars_nf = ["b10cm_", "b30cm_", "tcb_"]
vars_main = [vars_nf]

vars_names_nf = ["b10cm", "b30cm", "tcb"]
vars_names_main = [vars_names_nf]

storage_root = "/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201020_CMIP5_Matrix" # storage_root = "/pf/b/b380694/Regional/Output_Data/Macroecological" # Assuming function is run from home directory
storage_run = ['ipsl-cm5a-lr','gfdl-esm2m'] # storage_run = ['ipsl-cm5a-lr','gfdl-reanalysis', 'gfdl-esm2m']
storage_fishing = ['no-fishing']

for f in range(0, len(run)):
    curr_run = run[f]
    root = roots[f]
    
    if curr_run == 'gfdl-reanalysis':
        this_dirs = ['historical']
    else:
        this_dirs = dirs

    for h in range(0, len(this_dirs)): # Loop over historical or future scenarios
        curr_dir = this_dirs[h]
        curr_sub_dir = sub_dirs[h]
    
        curr_folder = root + curr_run + '/' + curr_dir + '/' 
    
        for i in range(0, len(curr_sub_dir)): # Loop over climate scenario
            curr_clim = curr_sub_dir[i]
        
            curr_files = glob.glob(curr_folder + '*' + curr_clim + '*')
        
            ## Get total time for this climate scenario
            curr_file_time = xr.open_mfdataset(curr_files[0], decode_times=False)      
            num_time = len(curr_file_time['time'])
        
            for j in range(0, len(fishing)): # Loop over fishing scenario
                curr_fishing = '*' + fishing[j] + '*'
                vars = vars_main[j]
                vars_names = vars_names_main[j]
            
                ## Pull out curr_fishing variables
                curr_fishing_files = fnmatch.filter(curr_files, curr_fishing)
            
                for k in range(0, num_areas): # Loop over region
                    curr_area = k + 1
                    curr_area_name = name_areas[k]
                
                    temp_store =  np.zeros([num_time, len(vars)])
            
                    col_names = []
                
                    for m in range(0, len(vars)): # Loop over variables
                        curr_var_search = '*' + vars[m] + '*'
                        curr_var_name = vars_names[m]
                        curr_file = fnmatch.filter(curr_fishing_files, curr_var_search)
                    
                        CURR_NCFILE = xr.open_mfdataset(curr_file, decode_times=False)
                        curr_var_units = CURR_NCFILE[curr_var_name].units
                        col_names.append(curr_var_name + '( ' + curr_var_units + ')')
        
                        for n in range(0, num_time): # Loop over time
                            perc_comp = str(round(100*n/num_time, 2))
                            print("Now working on " + curr_var_name + 'with climate forcing ' + curr_run + ' for model ' + model[0] + ", under climate forcing " + curr_clim \
                                  + ", for the " + curr_area_name + ', under a ' + fishing[j] + ' scenario' + \
                                  ' (' + perc_comp + '%)')

                            VAR = CURR_NCFILE[curr_var_name][n,:,:].values
                            
                            if k == 4:
                                mask_array = np.array((cmask == curr_area) | (cmask == curr_area+1), dtype = bool)
                            else:
                                mask_array = np.array(cmask == curr_area, dtype = bool)   
                            
                            VAR_REGION = VAR[mask_array]
                            AREA_REGION = tarea[mask_array]
                            indices = ~np.isnan(VAR_REGION)
                            temp_store[n,m] =  np.ma.average(VAR_REGION[indices], weights=AREA_REGION[indices])                       
                        
                    
                    ### Save csv for current climate scenario, fishing scenario and region
                    ### Extract range of dates
                    date_range = re.split("[_.]", curr_file[0])## What's the range of dates from the title of the last nc4?
                    start_date = date_range[len(date_range)-3] ## What's the start date?
                    end_date = date_range[len(date_range)-2] ## What's the finish date?
                    
                    starting = datetime.strptime(str(start_date), '%Y') ## Convert start date into datetime object
    
                    ## Get all month_year combos, e.g., Jan 2040 is "01_2040"
                    all_dates = []
                    for n in range(num_time):
                        currd = starting + n * relativedelta(years = 1)
                        all_dates.append(currd.strftime("%Y"))    
                    
                    date_frame = pd.DataFrame(all_dates, columns=["Date (yyyy)"])
    
                    #### Save forcing
                    # Convert to pd dataframe, to assign colnames and rownames
                    
                    temp_frame = pd.DataFrame(temp_store, columns=col_names)
                    temp_frame = pd.concat([date_frame, temp_frame], axis = 1)
                
                    # Get csv file name from last variable file name
                    bit_name = re.split('[/.]', curr_file[0])
                    bit_name = bit_name[len(bit_name)-2]
                    replacing = curr_var_name + '_global'
                    file_name = bit_name.replace(replacing, curr_area_name) + '.csv'
                
                    csv_file_name = storage_root + '/' + storage_run[f] + '/' + storage_fishing[j] + '/' + file_name
                
                    # Save file
                    temp_frame.to_csv(csv_file_name, index = False)
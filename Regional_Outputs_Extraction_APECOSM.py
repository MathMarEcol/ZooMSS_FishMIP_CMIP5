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

mask = '/pf/b/b380694/fishMIP_regional_mask3_outputs.nc'
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

num_areas = np.nanmax(cmask_INDEX) # How many areas are there? (We know there are 7)
num_areas = num_areas.astype(int)# Convert to integer from numpy array
name_areas = ["Humboldt", "North_Sea", "Mediterranean", "Baltic_Sea", "SE_Australia", "East_Bass_Strait", "Cook_Strait"] # Names of the seven areas

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
#### APECOSM
###########################################
###########################################
model = ['APECOSM']    

root = '/mnt/lustre01/work/bb0820/ISIMIP/ISIMIP2b/OutputData/marine-fishery_global/APECOSM/'
run = 'ipsl-cm5a-lr'
dirs = ['historical', 'future']
fishing = ['no-fishing']

sub_dirs_h = ["hist"]
sub_dirs_f = ["rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5"]
sub_dirs = [sub_dirs_h, sub_dirs_f]

vars = ["b10cm", "b30cm", "tcb"]
var_names = ['b10cm', 'b30cm','tcb']

storage_root = "./Regional/Output_Data/APECOSM" # Assuming function is run from home directory
storage_run = "ipsl-cm5a-lr"

for h in range(0, len(dirs)): # Loop over historical or future scenarios
    curr_dir = dirs[h]
    curr_sub_dir = sub_dirs[h]
    
    curr_folder = root + run + '/' + curr_dir + '/' 
    
    for i in range(0, len(curr_sub_dir)): # Loop over climate scenario
        curr_clim = curr_sub_dir[i]
        
        curr_files = glob.glob(curr_folder + '*' + curr_clim + '*')
        
        ## Get total time for this climate scenario
        curr_file_time = xr.open_mfdataset(curr_files[0], decode_times=False)      
        num_time = len(curr_file_time['time'])
        
        for j in range(0, len(fishing)): # Loop over fishing scenario
            curr_fishing = '*' + fishing[j] + '*'
            
            ## Pull out curr_fishing variables
            curr_fishing_files = fnmatch.filter(curr_files, curr_fishing)
            
            for k in range(0, num_areas): # Loop over region
                curr_area = k + 1
                curr_area_name = name_areas[k]
                
                temp_store =  np.zeros([num_time, len(vars)])
                
                num_years = int(num_time/12)
                ann_temp_store = np.zeros([num_years, len(vars)])
            
                col_names = []
                
                for m in range(0, len(vars)): # Loop over variables
                    curr_var_search = '*' + vars[m] + '*'
                    curr_var_name = var_names[m]
                    curr_file = fnmatch.filter(curr_fishing_files, curr_var_search)
                    
                    CURR_NCFILE = xr.open_mfdataset(curr_file, decode_times=False)
                    curr_var_units = CURR_NCFILE[curr_var_name].units
                    col_names.append(curr_var_name + '( ' + curr_var_units + ')')
        
                    for n in range(0, num_time): # Loop over time
                        perc_comp = str(round(100*n/num_time, 2))
                        print("Now working on " + curr_var_name + ' for model ' + model[0] + ", under climate forcing " + curr_clim \
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
                        
                    for q in range(0, num_years): # Loop over years
                        print("Now working on year averages for " + curr_var_name + 'with climate forcing ' + run + ' for model ' + model[0] + ", under climate forcing " + curr_clim \
                                  + ", for the " + curr_area_name + ', under a ' + fishing[j] + ' scenario' + \
                                  ' (' + perc_comp + '%)')
                        start_month = q*12
                        end_month = q*12 + 12
                        curr_years = range(start_month, end_month, 1)
                        curr_rows = temp_store[curr_years,:]
                        ann_temp_store[q,:] = np.mean(curr_rows, axis = 0)                        

                ### Save csv for current climate scenario, fishing scenario and region
                ### Extract range of dates
                date_range = re.split("[_.]", curr_file[0])## What's the range of dates from the title of the last nc4?
                start_date = date_range[len(date_range)-3] ## What's the start date?
                end_date = date_range[len(date_range)-2] ## What's the finish date?
                    
                starting = datetime.strptime(str(start_date), '%Y') ## Convert start date into datetime object
    
                ## Get all month_year combos, e.g., Jan 2040 is "2040.01"
                all_dates = []
                for n in range(num_time):
                    currd = starting + n * relativedelta(months = 1)
                    all_dates.append(currd.strftime("%Y.%m"))    
                    
                date_frame = pd.DataFrame(all_dates, columns=["Date (yyyy.mm)"])
                    
                ## Get all year combos, e.g., 2040 is "2040"
                all_years = []
                for n in range(num_years):
                    currd = int(start_date) + n
                    all_years.append(currd)    
                    
                year_frame = pd.DataFrame(all_years, columns=["Date (yyyy)"])
    
    
                #### Save forcing
                # Convert to pd dataframe, to assign colnames and rownames
                    
                temp_frame = pd.DataFrame(temp_store, columns=col_names)
                temp_frame = pd.concat([date_frame, temp_frame], axis = 1)
                
                temp_year_frame = pd.DataFrame(ann_temp_store, columns = col_names)
                temp_year_frame = pd.concat([year_frame, temp_year_frame], axis = 1)
                    
                # Get csv file names from last variable file name
                bit_name = re.split('[/.]', curr_file[0])
                bit_name = bit_name[len(bit_name)-2]
                replacing = curr_var_name + '_global'
                file_name = bit_name.replace(replacing, curr_area_name) + '.csv' # Monthly name
                year_file_name = file_name.replace('_monthly', '_annual') + '.csv' # Yearly name
                
                csv_file_name = storage_root + '/' + storage_run + '/' + fishing[0] + '/' + file_name
                csv_file_name_year = storage_root + '/' + storage_run + '/' + fishing[0] + '/' + year_file_name
                    
                # Save files
                temp_frame.to_csv(csv_file_name, index = False) # Monthly
                temp_year_frame.to_csv(csv_file_name_year, index = False) # Yearly




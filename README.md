vst overloads the plot() function, so to generate the frameset you will need to 
construct an appropriate input object. 
You do this with simple wrapper for list constructor, vst_list. 

Input list needs to have the following elements:

data        : a source dataframe
n           : how many rows to plot? (sql limit)
offset      : from which row to start?
title       : plot's main title
dir         : path to the directory where the frames will be written to
file        : file prefix (vst will automatically add numbers needed for easy ffmpeg use, e.g. file_0001.jpg, file_0002.jpg etc.)
width       : frame width in pixels
height      : ..
latitude    : name of the column storing latitude values
longitude   : ..
size        : name of the column specifying the dot cex
size_text   : name of the column specifying the dot cex as used in the legend
color       : name of the column specifying the dot color
color_text  : name of the column specifying the dot color as used in the legend
mc.cores    : [optional] how many cores should be used for plotting?; defaults to detectCores()
col_bg      : [optional] background color; defaults to 'black'
col_txt_1   : [optional] font color; defaults to rgb(1,1,1,.5)
marLim      : [optional] list of additional xlim/ylim coordinate offsets; defaults to list(x = c(0, 0), y = c(0, 0))
font        : [optional] defaults to 'Roboto'
font_light  : [optional] defaults to 'Roboto Light'
fadeout_duration : [optional] how many frames should be used for the fadeout effect?; defaults to 30
color_palette    : [optional] defaults to heat.colors(unique(data[[color]]))
size_cex_init    : [optional] initial cex amplification of the new dot/event appearing in the video; defaults to 2

Examples (you can find the dataframes in the ./data directory):

```r

source('vst.R')

load(file='data/uk12.rda')
uk = vst_list(data = ukd, 
              n = 10**3, 
              offset = 1, 
              title = 'Road accidents in the United Kingdom', 
              dir = 'framesets/uk12', 
              file = 'uk12', 
              width = 1000, 
              height = 1000, 
              latitude = 'Latitude', 
              longitude = 'Longitude', 
              size = 'Number_of_Vehicles', 
              size_text = 'Vehicles', 
              color = 'Number_of_Casualties', 
              color_text = 'Casualties', 
              mc.cores = 4, 
              col_bg = 'black', 
              col_txt_1 = rgb(1,1,1,.5))
}
plot(uk)

load(file='data/us12.rda')
us = vst_list(data = usd, 
              n = 10**3, 
              offset = 1, 
              title = 'Fatal road accidents in the United States', 
              dir = 'framesets/us12', 
              file = 'us12', 
              width = 1700, 
              height = 1000, 
              latitude = 'LATITUDE.N.16.8', 
              longitude = 'LONGITUD.N.16.8', 
              size = 'VE_TOTAL.N.16.0', 
              size_text = 'Vehicles', 
              color = 'FATALS.N.16.0', 
              color_text = 'Fatal\ncasualties', 
              mc.cores = 4, 
              col_bg = 'black', 
              col_txt_1 = rgb(1,1,1,.5), 
              marLim = list(x = c(-5, 0), y = c(0, 10)), 
              fadeout_duration = 100)
plot(us)

```

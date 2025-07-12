****** REPLICATION MATERIAL - The life cycle of conspiracy theories: 
****** Evidence from a long-term panel survey on conspiracy beliefs in Italy
****** M. Mancosu - S. Vassallo

cd "PATH_TO_DIR"

import delimited "figure2_data.csv",clear

******************************************************************
*********************** DATA MANAGEMENT **************************
******************************************************************

split date, p("-")
rename date1 year
rename date2 month
rename date3 day

destring year,gen(year_)
destring month,gen(month_)
destring day,gen(day_)

gen edate = mdy(month_, day_, year_)


***********************************************************
*********************** FIGURE 2 **************************
***********************************************************

graph twoway (lowess p_cosp edate ,bw(0.6)) (lowess p_comp edate ,bw(0.6))
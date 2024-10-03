install.packages("tidyverse")
install.packages("ggplot2")

library("tidyverse")
library("ggplot2")

# define the link to the data - you can try this in your browser too.  Note that the URL ends in .txt.
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"

#the next line tells the NASA site to create the temporary file
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")

# the next lines download the data
temp=read_csv(dataurl, 
              na="999.90", # tell R that 999.90 means missing in this dataset
              skip=1, # we will use our own column names as below so we'll skip the first row
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
# renaming is necessary becuase they used dashes ("-")
# in the column names and R doesn't like that.

#explore the data
View(temp)
glimpse(temp)

#New temp
temp_clean <- na.omit(temp)

#Develop the graphic
ggplot(temp_clean, aes(x = YEAR, y = JJA)) +
  geom_path() +
  geom_smooth() +
  xlab("Year") +
  ylab("Mean Summer Temperatures(C)") +
  ggtitle("Mean Summer Temperatures in Buffalo, NY\nSummer includes June, July, and August\nData from the Global Historical Climate Network\nRed line is a LOESS smooth")






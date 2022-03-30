## Chapter 3: Data Transformation for real-world WMS dataset

This is a code example to transform real work warehouse management system (WMS) data to a format that can be used for regression models. While the initial dataset includes information about picking places, e.g., a Q_PLATZ column indicating the identification number of a source location, this information can not be used for a regresssion analysis.

At this point, we need to translate the real-world dataset to a format where one variable (dependent variable, independent variables, or control variables) are represented by one column. 

Important: We do not change the data aggregation level which remains one pick operation at a given clock time. In case we would like to aggregate higher, we can use  pivot-like operations in R, e.g., by applying `group_by` or `summarize`.

### What we will learn in this chapter

In this chapter we will deal with data cleaning and data preperation for order picking data. WMS store extensive data logs for past events and order picking processes. However, before we can use these data for statistical analyis, we need to do some preperations.


### R packages

Several packages are recommended to be installed before starting with the analyses in this seminar. If you are not sure how to install a package in R, please check **00_basics** for further information.

```
library("dplyr")                                                
library("plyr")                                                 
library("readr")  
library("readxl")
library("tidyr")
library("corrplot")
library("data.table")
library("eha")
library("flexsurv")
library("fs")
library("ggplot2")
library("ggpubr")
library("JM")
library("lattice")
library("plyr")
library("Rcpp")
library("reshape2")
library("readxl")
library("rms")
library("splines")
library("stargazer")
library("survival")
library("survminer")
library("tidyverse")
library("sjPlot")
library("sjmisc")
library("sjstats")
library("stats")
library("lme4")
library("haven")
library("MuMIn")
library("devtools")
library("corrplot")
library("data.table")
library("eha")
library("flexsurv")
library("fs")
library("ggplot2")
library("ggpubr")
library("JM")
library("lattice")
library("PerformanceAnalytics")
library("plyr")
library("Rcpp")
library("readxl")
library("reshape2")
library("rms")
library("splines")
library("stargazer")
library("survival")
library("survminer")
library("tidyverse")
library("mosaicData")
library("dplyr")
library("treemapify")
library("scales")
library("ggcorrplot")
library("factoextra")
library("superheat")
library("texreg")
library("readxl")
library("dplyr")
library("vtable")
library("plm")
library("PerformanceAnalytics")
library("tidyverse")
library("corrplot")
library("Hmisc")
library("stargazer")
library("DataEditR")
library("pivottabler")
```

### Import several datasets and create one dataframe (the folder may vary on your computer)
In a first step, we load all the dataset from the companies WMS. They are stores on a path on you computer. In the example, the path is "D:/raw data for science/01_order picking datasets/04_order picking data_WSL_051/Jaspar automatic exports/01_01". Please not that this path may vary on your computer. After giving the path information to R, we combine all Microsoft Excel files to one dataframe. This dataframe is called WSL_all.

You could also use a special Microsoft Excel function called Power Query. As this is not part of the seminar you can lean more about here: https://support.microsoft.com/de-de/office/informationen-zu-power-query-in-excel-7104fbee-9e62-4cb9-a02e-5bfb1a6c536a.

```
WSL_all <- list.files(path = "D:/raw data for science/01_order picking datasets/04_order picking data_WSL_051/Jaspar automatic exports/01_01",    
                   pattern = "*.xlsx",
                   full.names = TRUE) %>% 
                   lapply(read_excel) %>%                                           
                   bind_rows  
```

Before we modify the dataframe we imported in R, lets have a look at the initial WMS data logs.

```
view(WSL_all)
```

### Modify dataframe
In a second step, we transform several infromation within the WMS dataset to usable data colums. One example is the Q_PLATZ colum recording the pick location. In the inital dataset, Q_PLATZ has 13 digits where combinations have different meaning. Note that a translation requires company-own knowledge that you may get though expert interviews. 

In our example, digits 1, 2, and 3 of Q_PLATZ include the warehouse number. Therefore, we formulate 'warehouse = substr(Q_PLATZ, 1, 3)'. This means that in the new dataset 'WSL100', we create a new column which we call 'warehouse'. This new column shall contain the first three digints of 'Q-PLATZ' starting at the fist digit (Q_PLATZ, **1**, 3) and ending at the third digit (Q_PLATZ, 1, **3**).

```
WSL100 <- transform(WSL_all, 
                   warehouse = substr(Q_PLATZ, 1, 3), 
                   area = substr(Q_PLATZ, 4, 5), 
                   aisle = substr(Q_PLATZ, 6, 7),
                   houese = substr(Q_PLATZ, 8, 9),
                   place = substr(Q_PLATZ, 10, 11),
                   picklocation = substr(Q_PLATZ, 8, 11),
                   level = substr(Q_PLATZ, 12, 13),
                   roellchen = substr(Q_PLATZ, 10, 10),
                   article = substr(ARTIKELNR, 1, 7),
                   packes_SKU = substr(ARTIKELNR, 8, 11))
```

### Transfer time and calculate seconds
In production and logistics systems, throuput time or process time is (often) the variable of interest. Logistics managers are often interested in picks per hour as a simple two dimensional measure for order picking system productivity. However, the initial dataset does not contain the time per pick on our data aggregation level. The WMS data records the start 'BEGINN_ZEIT' and end time 'ENDE_ZEIT' of each picking operations. 

(a) Convert the numerical colums to a time format. This is nessesary for step (b). If you miss this set, the output of your calculations in (b) may result in NA entries. 
```
WSL100$BEGINN_ZEIT  <- as.ITime(WSL100$BEGINN_ZEIT)
WSL100$ENDE_ZEIT    <- as.ITime(WSL100$ENDE_ZEIT)
```

(b) Calcualte time per pick as a difference of two clock times, the end time 'ENDE_ZEIT' minus the start time 'BEGINN_ZEIT' of the pick operation. The new entry for the time per pick is a column that we name 'picktime'.
```
WSL100              <- WSL100 %>% mutate(picktime = ENDE_ZEIT - BEGINN_ZEIT)
```

(c) Convert the 'picktime' column to a numeric format as we want to apply a regression model with several numeric variables.
```
WSL100$picktime     <- as.numeric(WSL100$picktime)
```


### Sort the dataset
In a last step, we sort the dataset according to the following logic (1) date of the pick, (2) picker identification number, (3) number of batch, and (4) the position of a pick within a batch. Here, we first make sure that important variables are in a numeric format (a) and then we run our sorting algorithm (b) for the entire dataset named 'WSL100'

(a) Convert data to numeric format for sorting algorithm.
```
WSL100$PERS_NR      <- as.numeric(WSL100$PERS_NR)
WSL100$AUFTRAGSNR   <- as.numeric(WSL100$AUFTRAGSNR)
WSL100$LFDNR        <- as.numeric(WSL100$LFDNR)
WSL100$ARTIKELNR    <- as.numeric(WSL100$ARTIKELNR)
```

(b) Formulate and run the sorting algorithm. Here, we use the following logic (1) date of the pick, (2) picker identification number, (3) number of batch, and (4) the position of a pick within a batch. Not that you can change the order of the sorting algorith simply by the order in the code. Additionally, you may add further sorting variables.

```
WSL100 <- WSL100[order(WSL100$NEU_DATUM, WSL100$PERS_NR, WSL100$AUFTRAGSNR, WSL100$AUFTRAGSPOS), ]
```

### Export table (insert your individual folder to save the final dataset)
Finally, we export our dataset in two steps. First, we set a directory where we want R to save our dataframe as a Microsoft Excel file 'setwd'. Second, we call the export funtion on the dataframe 'WSL100' and name the file with 'file="01_01.csv"' which we want to save in **D:/raw data for science/(...)** 

```
setwd("D:/raw data for science/01_order picking datasets/04_order picking data_WSL_051/Jaspar automatic exports")
write.csv2(WSL100, file="01_01.csv")
```

With this algorithm, we can transform as many WMS datasets as we want to a format for further statistical analysis.

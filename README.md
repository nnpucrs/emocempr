# emocempR Package
## What it is?
This is a package for internal use by the Multicentric Study of Pediatric Multiple Sclerosis (EMOCEMP) investigators.
Here, we provide functions that facilitate manipulating data directly extracted from qualtrics in the *csv* numeric format.
Moreover, this functions are used to generate reproducible and automated analysis, as well as an automated dashboard for the purpose of investigator consult.

## How to install
To install the emocempr package to as following in your R console.

```
library(devtools)
install_github("rafaelsommer1/emocempr")
```

We suggest you update from the source code each time you wish to use the package, since it is a project on development.
For that, code as following:

```
install_github("hrbrmstr/dtupdate")
library(dtupdate)
github_update()
```
## Common applications
All functions are based on exported data from  qualtrics forms with this configuration (csv numeric)

![](https://i.imgur.com/GQHBnN7.png)

After that, you can start using the package functions, usually, all data exported from qualtrics needs to be cleaned so it can be analyzed. The package provides functions which perform this in basically all EMOCEMP forms, so for eg, given a Visit 1 form:

```
v1 <- clean_v1("file.csv")
```
This read the csv file downloaded from qualtrics, clean it and store the results in a R object: ```v1```. You can perform analysis directly in R using this object or write a csv file from the cleaned data in order to analyze other packages such as GraphPad, SPSS, Excel...

If this is inteded, something like:
```
write.csv2(v1, "filename")
```
Should do it.

Further, there are package functions that allow for data manipulation in R. So, in case you wanted to bind multiple visits data you can use the ```merge_visits``` function
```
v1 <- clean_v1("fileV1.csv")
v2 <- clean_v2("fileV2.csv")
v3 <- clean_v3("fileV3.csv")
merged <- merge_visits(v1,v2,v3, mode = "long", all.patients=FALSE)
```
This will use *CLEANED* data from visits 1, 2 and 3, combine them into a "long" format (each observation as a line) and only for patients who have data collected from all 3 visits (all.patients=FALSE), and store the results into the merged object.

As before, one may convert this object to a csv file for analyzing in other software or follow the analysis whitin R.

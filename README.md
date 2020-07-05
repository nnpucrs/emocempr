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

After that, you can start using the package functions, usually, all data exported from qualtrics needs to be cleaned so it can be analyzed. The package provides functions which perform this in basically all EMOCEMP forms, so for eg:

```
v1 <- clean_v1(file.csv)
```
This read the csv file downloaded from qualtrics, clean it and store the results in a R object: ```v1```. You can perform analysis directly in R using this object or write a csv file from the cleaned data in order to analyze other packages such as GraphPad, SPSS, Excel...

If this is inteded, something like:
```
write.csv2(v1, "filename")
```
Should do it.

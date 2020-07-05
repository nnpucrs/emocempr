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

[Imgur](https://i.imgur.com/GQHBnN7.png)


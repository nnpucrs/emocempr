# emocempr package
This is a package for internal use by the Multicentric Study of Pediatric Multiple Sclerosis (EMOCEMP) investigators.
Here, we provide functions that facilitate manipulating data directly extracted from qualtrics in the *csv* numeric format.
Moreover, this functions are used to generate reproducible and automated analysis, as well as an automated dashboard for the purpose of investigator consult.

To install the emocempr package to as following in your R console.

```
library(devtools)
install.github("rafaelsommer1/emocempr")
```

We suggest you update from the source code each time you wish to use the package, since it is a project on development.
For that, code as following:

```
install_github("hrbrmstr/dtupdate")
library(dtupdate)
github_update()
```

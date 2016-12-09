# ezdev-src
Package {ezdev} provides convenient addin functionality to R Studio to make your life easier as a package developer. Addin functionality is available from the "Addins" menu in R Studio and keyboard shortcuts can be created for commonly used functions. Currently three addins are provided: (1) DF - create a small random sample data.frame for quick testing of functions; (2) docHeader - creates a file document comment header populated with file name, project, package, author, description, and date modified attributes; (3) funcHeader - Creates a {roxygen2} function skeleton that can be easily modified. 

If you just want to *use* the package:
```
install.packages("devtools")
devtools::install_github("databrotherhood/ezdev-src")
library("ezdev")
```

If you want to help develop the package:
1.) Fork the repository
2.) Clone your fork
3.) Open `ezdev.Rproj` in R Studio


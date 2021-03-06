ucbudar
=====

an R package with everyday functions for my job. Forked from muadc.

## Variables

Contains various variables:

* currentFY  - current fiscal year
* fyStartDate - the date the current fiscal year started
* fyEndDate - the date the current fiscal year ended
* tmuGoal - the dollar goal for Transform MU
* afGoal - the dollar goal for the Millikin Fund


## Aliases

Contains the following aliases, primarily for interactive use:

* v - View()
* h - head()
* s - summary()
* cd - setwd()


## Functions

* binary.test - checks to see if a column is empty, returns a data frame of key/value pairs with a boolean factor variable
* coalesce - an SQL-style coalesce function to return the first non-NA value in an assortment of vectors
* eightytwenty - find the 80/20 cutoff in a vector
* grepnames - search column names for a string
* histrug - plot a histogram with a rug
* initcommitsdb - initalizes dplyr-style connection to my local MySQL giving datawarehouse
* is.blank - an R-style is.foo test to see if a field is blank, purposefully more generalized than `is.na`
* muplot - sets formatting options for plots
* outliers - find the upper and lower bound for outliers
* read.hallp - read hallp.csv, optionally looking to see if it exists
* read.tidy - read a csv & tidy up column names, optionally looking to see if the object exists
* renormalize - a function to renormalize (in the Bryce-Codd sense of normalization) a set of key/value pair where the value contains multiple values
* renormalize - turn a denormalized list of values back into id/value pairs
* startup - do various typical startup tasks. Largely useless
* view.sample - get a quick look at data, particularly useful for very wide data frames
* write.clip - write an object to the clipboard

## TODO 
* givingfactor - a function to bin giving (or wealth capacity) into various factor levels
* startup - revise, make useful or remove
* renormalize - lots of edge cases need to be considered
* something's wonky with the documentation -- package will build, but won't check. 

New
----------------------
- CDW connections
- CDW table connections
- ROracle dependencies? 


Revise
----------------------
- apply.mail.exclusions
- apply.email.exclusions
- addspouses
- dupstatus
- getprimaries
- ggmutheme
- giftandmemos
- muadc.package
- validate-lists
- dbReconnect needs a way to reconnect user-created db tibbles



Remove
-----------------
- data/oldhallpnames
- startup
- RMYSQL dependencies

x appstdi-to-df
x bannerize-pidm
x createhallpwitholdname
x initcommitdb
x muplot 
x variables


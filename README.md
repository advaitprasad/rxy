# ryx


This function computes correlation coefficients and their associated p-values
between a dependent variable and specified independent variables.


![Image](photo.png)

## Installation
You can install this package with the following code:
``` 
if(!require(remotes)){
 install.packages("remotes")}
remotes::install_github("advaitprasad/ryx")
```
## Example
This is a basic example which shows you how to describe a data frame:
``` 
library(ryx)
df_info<- ryx(mtcars)
df_info
plot(df_info)
```

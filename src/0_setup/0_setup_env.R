#renv::init()
Sys.setenv(RETICULATE_PYTHON = "C:/Users/espad/Anaconda3/envs/us-census-env/python.exe")
library(reticulate)

#Instruct reticulate and renv which conda env to use

#Instruct reticulate to use conda virtual env
use_condaenv("us-census-env", required = TRUE)

#Check condaenv is correct
py_config()

#Instruct renv to manage condaenv loaded by reticulate
renv::use_python("C:/Users/espad/Anaconda3/envs/us-census-env/python.exe")

renv::snapshot()

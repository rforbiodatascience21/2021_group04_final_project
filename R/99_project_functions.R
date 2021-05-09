# Define project functions ------------------------------------------------

scale_function <- function(x){
  asinh_scale <- 150
  asinh(x/asinh_scale)
}



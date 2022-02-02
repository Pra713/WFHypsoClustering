# This file contains small helper functions

library(tidyverse)

scaleFun <- function(x)
{
  y <- vector()
  for (x_each in x)
  {
    y <- c(y, as.character(x_each))
  }
  return (y)
}

# Folder for saving
foldered <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename)
}

# Folder for saving png
foldered_png <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename, '.png')
}

# Folder for saving pdf
foldered_pdf <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename, '.pdf')
}

# Folder for saving csv
foldered_csv <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename, '.csv')
}

# Function to display scientific notation in ggplot
fun_fancy_scientific <- function(given_number_original) 
{
  # print (given_number_original)
  if (is.na(given_number_original))
  {
    # print (NA)
    return (NA)
  } else if (given_number_original == 0)
  {
    # print ("0")
    return (0)
  } else if (given_number_original == 1)
  {
    # print ("1")
    return (1)
    # } else if (given_number_original == 10)
    # {
    #   print ("10")
    #   return (10)
  } else
  {
    # print ("Normal Number")
    # turn in to character string in scientific notation
    given_number <- format(given_number_original, scientific = TRUE)
    if(str_sub(given_number, 1, 1) == '1' & str_sub(given_number, 2, 2) == 'e')
    {
      # quote the part before the exponent to keep all the digits
      given_number <- gsub("^(.*)e", "e", given_number)
      # remove + in exponent
      given_number <- gsub("e\\+","e",given_number)
      # turn the 'e+' into plotmath format
      given_number <- gsub("e", "10^", given_number)
    }else
    {
      # quote the part before the exponent to keep all the digits
      given_number <- gsub("^(.*)e", "'\\1'e", given_number)
      # remove + in exponent
      given_number <- gsub("e\\+","e",given_number)
      # turn the 'e+' into plotmath format
      given_number <- gsub("e", "%*%10^", given_number)
    }
    # print ("Done")
    # return this as an expression
    return (parse(text=given_number))
    # ?parse
  }
}
fun_fancy_scientific <- Vectorize(fun_fancy_scientific)
# fun_fancy_scientific(1000)
# fun_fancy_scientific(1)
# fun_fancy_scientific(10)
# fun_fancy_scientific(1500)
# fun_fancy_scientific(3500)
# fun_fancy_scientific(1000)
# fun_fancy_scientific(0.005)
# fun_fancy_scientific(0.001)
# fun_fancy_scientific(0.00105)

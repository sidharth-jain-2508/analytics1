library("readxl")
library("dplyr")

?read_excel

read_excel(path, sheet = NULL, range = NULL, col_names = TRUE,
           col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
           guess_max = min(1000, n_max))

NPS_Data <- read_excel(path="C:/Users/ekratsh/Desktop/Training/Logistic Regression on NPS/NPS Data.xlsx", sheet = 'Sheet1',col_names = T)

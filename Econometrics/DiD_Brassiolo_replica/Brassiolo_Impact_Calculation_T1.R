library(dplyr)
library(tidyverse)
library(haven)

ttest_significance <- function(ttest_pvalue) {
  # Funcion que define un valor de significancia estadística con base en su
  # p-value
  significance <- ""
  if (ttest_pvalue < 0.01) {
    significance <- "***"
  } else if (ttest_pvalue < 0.05) {
    significance <- "**"
  } else if (ttest_pvalue < 0.1) {
    significance <- "*"
  }
  
  return(significance)
}

# Prueba la funcion
ttest_significance(0.001)
ttest_significance(0.04)
ttest_significance(0.05)
ttest_significance(0.99)

# Importa el dataset
# Nota: Se importa el dataset con .dta porque contiene variables categoricas
# con etiquetas, es decir, una varible de tipo string es equivalente con
# un número. Esto es necesario para replicar las condicionales en el código
# de STATA.
data <- read_dta("@Data/macrosurvey_final.dta") 
 
# Tabla 1. Descriptive Statistics. Survey on Violence Against women
# *Self reported abuse
data$ipv_ly <- data$ipv_ly * 100
data$ipv_ly[data$marital == 1 & data$q5 == 2 
            & (data$sexual_v == 1 | data$physi_v == 1 | data$psycho1_v == 1
               | data$psycho2_v == 1)] <- NaN
data$ipv_ly[(data$marital == 1 | data$marital == 7)
            & data$couple == 0
            & data$q29 >= 2
            & data$q29 <= 99] <- NaN

# Technical abuse
varlist <- c("tech_v", "physi_v", "sexual_v", "psycho1_v", "psycho2_v")
for (var in varlist) {
  data[[var]][data$couple == 1] <- NaN # These variables are defined only for women with partner...
  data[[var]] <- data[[var]] * 100  
}

# Define la lista de variables
rownames <- c("ipv_ly", "tech_v", "physi_v", "sexual_v", "psycho1_v", "psycho2_v",
              "age1", "age2", "age3", "age4", "age5",
              "edu1", "edu2", "edu3", "edu4",
              "marital1", "marital2", "marital3", "marital4", "marital5", "marital6",
              "employed", "unemp", "olf", "couple", "duration", "children", "nchildren",
              "age_h",
              "edu_h1", "edu_h2", "edu_h3", "edu_h4")

rownames_desc <- c("Self-reported",
                   "Technical",
                   "Physical",
                   "Sexual",
                   "Psychological (control)",
                   "Psychological (emotional mistreatment)",
                   "18-29",
                   "30-39",
                   "40-49",
                   "50-59",
                   "60 or older",
                   "(Woman's) Primary or less",
                   "(Woman's) Lower high school",
                   "(Woman's) Upper high school",
                   "(Woman's) University",
                   "Single",
                   "Dating",
                   "Cohabiting",
                   "Married",
                   "Separated",
                   "Divorced",
                   "Employed",
                   "Unemployed",
                   "Out of labor force",
                   "In a relationship",
                   "Duration current relationship (years)",
                   "With children",
                   "No. of children",
                   "Age",
                   "(Partner's) Primary or less",
                   "(Partner's) Lower high school",
                   "(Partner's) Upper high school",
                   "(Partner's) University",
                   "Observations"
                   )

colnames_desc <- c("Total Sample (mean)",
                       "Total Sample (SD)",
                       "Sample Means (1999)",
                       "Sample Means (2002)",
                       "Sample Means (2006)",
                       "Difference in Means (2002 versus 1999)",
                       "Difference in Means (2006 versus 2002)"
                       )

ts_m <- mean(data$ipv_ly, na.rm = T) # Total sample mean
ts_d <- sd(data$ipv_ly, na.rm = T) # Total sample standard deviation
sm_99 <- mean(data$ipv_ly[data$year == 1999], na.rm = T) # Sample mean - 1999
sm_02 <- mean(data$ipv_ly[data$year == 2002], na.rm = T) # Sample mean - 2002
sm_06 <-mean(data$ipv_ly[data$year == 2006], na.rm = T) # Sample mean - 2006

# Get the difference between 2002 and 1999
dim_02_99 <- sm_02 - sm_99
ttest_02_99 <- t.test(data$ipv_ly[data$year == 2002],
                      data$ipv_ly[data$year == 1999])$p.value
dim_02_99_s <- sub("0.", ".",
                   paste0(round(dim_02_99, 3), ttest_significance(ttest_02_99)))
  
# Get the difference between 2006 and 2002
dim_06_02 <- sm_06 - sm_02
ttest_06_02 <- t.test(data$ipv_ly[data$year == 2006],
                      data$ipv_ly[data$year == 2002])$p.value
dim_06_02_s <- sub("0.", ".",
                   paste0(round(dim_06_02, 3), ttest_significance(ttest_06_02)))

table1 <- matrix(nrow = length(rownames)+1, ncol = 7)

for (i in seq(rownames)) {
  rowname <- rownames[i]
  
  ts_m <- round(mean(data[[rowname]], na.rm = T), 3) # Total sample mean
  ts_d <- round(sd(data[[rowname]], na.rm = T), 3) # Total sample standard deviation
  sm_99 <- round(mean(data[[rowname]][data$year == 1999], na.rm = T), 3) # Sample mean - 1999
  sm_02 <- round(mean(data[[rowname]][data$year == 2002], na.rm = T), 3) # Sample mean - 2002
  sm_06 <- round(mean(data[[rowname]][data$year == 2006], na.rm = T), 3) # Sample mean - 2006
  
  # Get the difference between 2002 and 1999
  dim_02_99 <- sm_02 - sm_99
  ttest_02_99 <- t.test(data[[rowname]][data$year == 2002],
                        data[[rowname]][data$year == 1999])$p.value
  dim_02_99_s <- sub("0.", ".",
                     paste0(round(dim_02_99, 3), ttest_significance(ttest_02_99)))
  
  # Get the difference between 2006 and 2002
  dim_06_02 <- sm_06 - sm_02
  ttest_06_02 <- t.test(data[[rowname]][data$year == 2006],
                        data[[rowname]][data$year == 2002])$p.value
  dim_06_02_s <- sub("0.", ".",
                     paste0(round(dim_06_02, 3), ttest_significance(ttest_06_02)))
  
  row <- c(ts_m, ts_d, sm_99, sm_02, sm_06, dim_02_99_s, dim_06_02_s)
  table1[i, ] <- row
}

# Lista para el numero de observaciones
observations <- c(dim(data)[1],
                  0,
                  dim(filter(data, year == 1999))[1],
                  dim(filter(data, year == 2002))[1],
                  dim(filter(data, year == 2006))[1],
                  0,
                  0
                  )

table1[34, ] <- observations

# Crea un dataframe
table1_df <- as.data.frame(table1)
colnames(table1_df) <- colnames_desc
rownames(table1_df) <- rownames_desc

view(table1_df)
write.csv(table1_df, "@Data/table1_R.csv")

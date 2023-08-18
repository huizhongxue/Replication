# Load the package
library(testinterference)
library(haven)
library(tidyverse)

setwd("D:/OneDrive/Boston University/Summer 2023/RA/general interference/1_Hoshino and Yanagi 2023 (w_data)/Hoshino and Yanagi replication/Social networks and the decision to insure/data")

# Getting Started with the testinterference Package
browseVignettes("testinterference")

# Testing Procedure: Fisher's Sharp Null Hypothesis
vignette("Fisher", package = "testinterference")

# Testing Procedure: SUTVA
vignette("SUTVA", package = "testinterference")

# Testing Procedure: Exposure 1
vignette("exposure1", package = "testinterference")

# Testing Procedure: Exposure 2
vignette("exposure2", package = "testinterference")


data <- read_dta("0422analysis.dta")

# Hypotheses:
# Ha vs Hc: SUTVA
# Hb vs. Hc: Exposure 1
# Hc vs. Hd: Exposure 2

info <- data %>% 
  group_by(village) %>% 
  summarize(count = n(),
            takeup = mean(takeup_survey), 
            int = mean(intensive),
            sec = mean(delay)) %>% 
  filter(count >= 50)

data1 <- data %>% filter(village %in% info$village)


# STUVA
Ha <- testinterference( Y                 = data1$takeup_survey,
                        Z                 = data1$intensive,
                        A                 = data1$A,
                        hypothesis        = "SUTVA",
                        method            = "MIS",
                        design            = "Bernoulli",
                        prob              = 0.5,
                        focal_unit        = NULL,
                        num_focal_unit    = NULL,
                        num_randomization = 999,
                        strata            = NULL,
                        zmatrix           = NULL,
                        kappa             = NULL,
                        cores             = 1)

# Exposure 1
Hb <- testinterference( Y                 = data1$takeup_survey,
                        Z                 = data1$intensive,
                        A                 = data1$A,
                        hypothesis        = "exposure1",
                        method            = "MIS",
                        design            = "Bernoulli",
                        prob              = 0.5,
                        focal_unit        = NULL,
                        num_focal_unit    = NULL,
                        num_randomization = 999,
                        strata            = NULL,
                        zmatrix           = NULL,
                        kappa             = NULL,
                        cores             = 1)


# Exposure 2
Hc <- testinterference( Y                 = data1$takeup_survey,
                        Z                 = data1$intensive,
                        A                 = data1$A,
                        hypothesis        = "exposure2",
                        method            = "MIS",
                        design            = "Bernoulli",
                        prob              = 0.5,
                        focal_unit        = NULL,
                        num_focal_unit    = NULL,
                        num_randomization = 999,
                        strata            = NULL,
                        zmatrix           = NULL,
                        kappa             = NULL,
                        cores             = 1)






Week 8 locomotion prediction in SenseOfSensors
================

  - [Data extraction, transformation and
    loading](#data-extraction-transformation-and-loading)
      - [Packages loaded](#packages-loaded)
      - [Data anonymisation](#data-anonymisation)
      - [Sensor data](#sensor-data)
          - [Cow features](#cow-features)
          - [Herd features](#herd-features)
      - [Locomotion data](#locomotion-data)
      - [BCS data](#bcs-data)
      - [Combined data](#combined-data)
  - [Random forest prediction locomotion score week
    8](#random-forest-prediction-locomotion-score-week-8)
      - [Create test and training data](#create-test-and-training-data)
      - [Model](#model)
          - [Variable importance](#variable-importance)
          - [Validation](#validation)
  - [Plots](#plots)
      - [Eating time](#eating-time)
      - [Rumination time](#rumination-time)
      - [Standups](#standups)

# Data extraction, transformation and loading

## Packages loaded

``` r
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

package_list <- c("readr", "dplyr", "plyr", "tidyr", "stringr", "data.table", "lubridate", "caret", "devtools", "tidyverse", "ggplot2", "pdp", "ROCR")

for (pkg in package_list) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {library(pkg, character.only = TRUE)}
  print(citation(pkg))
}
```

    ## 
    ## To cite package 'readr' in publications use:
    ## 
    ##   Hadley Wickham and Jim Hester (2020). readr: Read Rectangular Text
    ##   Data. R package version 1.4.0.
    ##   https://CRAN.R-project.org/package=readr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {readr: Read Rectangular Text Data},
    ##     author = {Hadley Wickham and Jim Hester},
    ##     year = {2020},
    ##     note = {R package version 1.4.0},
    ##     url = {https://CRAN.R-project.org/package=readr},
    ##   }

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## To cite package 'dplyr' in publications use:
    ## 
    ##   Hadley Wickham, Romain François, Lionel Henry and Kirill Müller
    ##   (2020). dplyr: A Grammar of Data Manipulation. R package version
    ##   1.0.2. https://CRAN.R-project.org/package=dplyr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {dplyr: A Grammar of Data Manipulation},
    ##     author = {Hadley Wickham and Romain François and Lionel {
    ##              Henry} and Kirill Müller},
    ##     year = {2020},
    ##     note = {R package version 1.0.2},
    ##     url = {https://CRAN.R-project.org/package=dplyr},
    ##   }

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## 
    ## To cite plyr in publications use:
    ## 
    ##   Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data
    ##   Analysis. Journal of Statistical Software, 40(1), 1-29. URL
    ##   http://www.jstatsoft.org/v40/i01/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {The Split-Apply-Combine Strategy for Data Analysis},
    ##     author = {Hadley Wickham},
    ##     journal = {Journal of Statistical Software},
    ##     year = {2011},
    ##     volume = {40},
    ##     number = {1},
    ##     pages = {1--29},
    ##     url = {http://www.jstatsoft.org/v40/i01/},
    ##   }
    ## 
    ## 
    ## To cite package 'tidyr' in publications use:
    ## 
    ##   Hadley Wickham (2020). tidyr: Tidy Messy Data. R package version
    ##   1.1.2. https://CRAN.R-project.org/package=tidyr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {tidyr: Tidy Messy Data},
    ##     author = {Hadley Wickham},
    ##     year = {2020},
    ##     note = {R package version 1.1.2},
    ##     url = {https://CRAN.R-project.org/package=tidyr},
    ##   }
    ## 
    ## 
    ## To cite package 'stringr' in publications use:
    ## 
    ##   Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for
    ##   Common String Operations. R package version 1.4.0.
    ##   https://CRAN.R-project.org/package=stringr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {stringr: Simple, Consistent Wrappers for Common String Operations},
    ##     author = {Hadley Wickham},
    ##     year = {2019},
    ##     note = {R package version 1.4.0},
    ##     url = {https://CRAN.R-project.org/package=stringr},
    ##   }

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## 
    ## To cite package 'data.table' in publications use:
    ## 
    ##   Matt Dowle and Arun Srinivasan (2020). data.table: Extension of
    ##   `data.frame`. R package version 1.13.2.
    ##   https://CRAN.R-project.org/package=data.table
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {data.table: Extension of `data.frame`},
    ##     author = {Matt Dowle and Arun Srinivasan},
    ##     year = {2020},
    ##     note = {R package version 1.13.2},
    ##     url = {https://CRAN.R-project.org/package=data.table},
    ##   }

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## 
    ## To cite lubridate in publications use:
    ## 
    ##   Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy
    ##   with lubridate. Journal of Statistical Software, 40(3), 1-25. URL
    ##   http://www.jstatsoft.org/v40/i03/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Dates and Times Made Easy with {lubridate}},
    ##     author = {Garrett Grolemund and Hadley Wickham},
    ##     journal = {Journal of Statistical Software},
    ##     year = {2011},
    ##     volume = {40},
    ##     number = {3},
    ##     pages = {1--25},
    ##     url = {http://www.jstatsoft.org/v40/i03/},
    ##   }

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## 
    ## To cite package 'caret' in publications use:
    ## 
    ##   Max Kuhn (2020). caret: Classification and Regression Training. R
    ##   package version 6.0-86. https://CRAN.R-project.org/package=caret
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {caret: Classification and Regression Training},
    ##     author = {Max Kuhn},
    ##     year = {2020},
    ##     note = {R package version 6.0-86},
    ##     url = {https://CRAN.R-project.org/package=caret},
    ##   }

    ## Loading required package: usethis

    ## 
    ## To cite package 'devtools' in publications use:
    ## 
    ##   Hadley Wickham, Jim Hester and Winston Chang (2020). devtools: Tools
    ##   to Make Developing R Packages Easier. R package version 2.3.2.
    ##   https://CRAN.R-project.org/package=devtools
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {devtools: Tools to Make Developing R Packages Easier},
    ##     author = {Hadley Wickham and Jim Hester and Winston Chang},
    ##     year = {2020},
    ##     note = {R package version 2.3.2},
    ##     url = {https://CRAN.R-project.org/package=devtools},
    ##   }

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v tibble  3.0.4     v forcats 0.5.0
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x plyr::arrange()          masks dplyr::arrange()
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x data.table::between()    masks dplyr::between()
    ## x purrr::compact()         masks plyr::compact()
    ## x plyr::count()            masks dplyr::count()
    ## x lubridate::date()        masks base::date()
    ## x plyr::failwith()         masks dplyr::failwith()
    ## x dplyr::filter()          masks stats::filter()
    ## x data.table::first()      masks dplyr::first()
    ## x lubridate::hour()        masks data.table::hour()
    ## x plyr::id()               masks dplyr::id()
    ## x lubridate::intersect()   masks base::intersect()
    ## x lubridate::isoweek()     masks data.table::isoweek()
    ## x dplyr::lag()             masks stats::lag()
    ## x data.table::last()       masks dplyr::last()
    ## x purrr::lift()            masks caret::lift()
    ## x lubridate::mday()        masks data.table::mday()
    ## x lubridate::minute()      masks data.table::minute()
    ## x lubridate::month()       masks data.table::month()
    ## x plyr::mutate()           masks dplyr::mutate()
    ## x lubridate::quarter()     masks data.table::quarter()
    ## x plyr::rename()           masks dplyr::rename()
    ## x lubridate::second()      masks data.table::second()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x plyr::summarise()        masks dplyr::summarise()
    ## x plyr::summarize()        masks dplyr::summarize()
    ## x purrr::transpose()       masks data.table::transpose()
    ## x lubridate::union()       masks base::union()
    ## x lubridate::wday()        masks data.table::wday()
    ## x lubridate::week()        masks data.table::week()
    ## x lubridate::yday()        masks data.table::yday()
    ## x lubridate::year()        masks data.table::year()

    ## 
    ##   Wickham et al., (2019). Welcome to the tidyverse. Journal of Open
    ##   Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Welcome to the {tidyverse}},
    ##     author = {Hadley Wickham and Mara Averick and Jennifer Bryan and Winston Chang and Lucy D'Agostino McGowan and Romain François and Garrett Grolemund and Alex Hayes and Lionel Henry and Jim Hester and Max Kuhn and Thomas Lin Pedersen and Evan Miller and Stephan Milton Bache and Kirill Müller and Jeroen Ooms and David Robinson and Dana Paige Seidel and Vitalie Spinu and Kohske Takahashi and Davis Vaughan and Claus Wilke and Kara Woo and Hiroaki Yutani},
    ##     year = {2019},
    ##     journal = {Journal of Open Source Software},
    ##     volume = {4},
    ##     number = {43},
    ##     pages = {1686},
    ##     doi = {10.21105/joss.01686},
    ##   }
    ## 
    ## 
    ## To cite ggplot2 in publications, please use:
    ## 
    ##   H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
    ##   Springer-Verlag New York, 2016.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Book{,
    ##     author = {Hadley Wickham},
    ##     title = {ggplot2: Elegant Graphics for Data Analysis},
    ##     publisher = {Springer-Verlag New York},
    ##     year = {2016},
    ##     isbn = {978-3-319-24277-4},
    ##     url = {https://ggplot2.tidyverse.org},
    ##   }

    ## 
    ## Attaching package: 'pdp'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     partial

    ## 
    ## To cite pdp in publications use:
    ## 
    ##   Brandon M. Greenwell (2017). pdp: An R Package for Constructing
    ##   Partial Dependence Plots. The R Journal, 9(1), 421--436. URL
    ##   https://journal.r-project.org/archive/2017/RJ-2017-016/index.html.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {pdp: An R Package for Constructing Partial Dependence Plots},
    ##     author = {Brandon M. Greenwell},
    ##     journal = {The R Journal},
    ##     year = {2017},
    ##     volume = {9},
    ##     number = {1},
    ##     pages = {421--436},
    ##     url = {https://journal.r-project.org/archive/2017/RJ-2017-016/index.html},
    ##   }
    ## 
    ## 
    ## To cite ROCR in publications use:
    ## 
    ## Sing T, Sander O, Beerenwinkel N, Lengauer T (2005). "ROCR: visualizing
    ## classifier performance in R." _Bioinformatics_, *21*(20), 7881. <URL:
    ## http://rocr.bioinf.mpi-sb.mpg.de>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     entry = {article},
    ##     title = {ROCR: visualizing classifier performance in R},
    ##     author = {T. Sing and O. Sander and N. Beerenwinkel and T. Lengauer},
    ##     year = {2005},
    ##     journal = {Bioinformatics},
    ##     volume = {21},
    ##     number = {20},
    ##     pages = {7881},
    ##     url = {http://rocr.bioinf.mpi-sb.mpg.de},
    ##   }
    ## 
    ## We have invested a lot of time and effort in creating ROCR, please cite
    ## it when using it for data analysis.

## Data anonymisation

Make sure you have a file with Anonymization.R which contains some salt
for the anonymisation

``` r
source("../Anonymization.R")
#anony
if (!require("anonymizer")) {
  devtools::install_github("paulhendricks/anonymizer")
  library(anonymizer)
}
```

    ## Loading required package: anonymizer

``` r
citation("anonymizer")
```

    ## Warning in citation("anonymizer"): no date field in DESCRIPTION file of package
    ## 'anonymizer'

    ## 
    ## To cite package 'anonymizer' in publications use:
    ## 
    ##   Paul Hendricks (2020). anonymizer: Anonymize Data Containing
    ##   Personally Identifiable Information. R package version 0.2.2.
    ##   https://github.com/paulhendricks/anonymizer
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {anonymizer: Anonymize Data Containing Personally Identifiable Information},
    ##     author = {Paul Hendricks},
    ##     year = {2020},
    ##     note = {R package version 0.2.2},
    ##     url = {https://github.com/paulhendricks/anonymizer},
    ##   }

``` r
anoCols <- c("HerdIdentifier", "AnimalIdentifier", "AnimalEartag")
```

## Sensor data

``` r
if(!(exists('AllDataRaw') && is.data.frame(get('AllDataRaw')))) {
  
  files <- list.files("../Data/AllSensorObservationsSarahKuipers07052020/")
  fullFiles <- paste(getwd(), "/Data/AllSensorObservationsSarahKuipers07052020/",files, sep="")
  
  AllDataRaw <- rbind.fill(lapply(fullFiles, fread, header=TRUE, stringsAsFactors = TRUE))

  AllData <- AllDataRaw %>% 
    dplyr::filter(
      LactationNumber > 1 &
      between(DaysInMilk, -21, 21) &
      !is.na(SensorValue)
    ) %>%
    mutate(
      Parity  = recode_factor(LactationNumber,
                              "2" = "2",
                              "3" = "3",
                              .default = "≥4"),
      HerdIdentifier =  as.factor(HerdIdentifier),
      AnimalIdentifier =  as.factor(AnimalIdentifier)
      ) %>% # Only select the necesarry columns
    dplyr::mutate_at(vars(anoCols), anonymizer::anonymize, .char = anoSalt)
}

save(AllData, file = "./Data/AllPredictionData.RData")
```

``` r
load("../Data/AllPredictionData.RData")
```

### Cow features

``` r
PredictionData <- AllData %>%
    dplyr::filter(
      between(DaysInMilk, -21, 20) &
      stringr::str_detect(SensorType, 'Rumination time|Eating time|Lying time|Standup')
      ) %>%
    dplyr::mutate(
                  CalvingDate = case_when(
                    DaysInMilk < 0 ~ as.Date(SubsequentCalvingDate, format="%Y-%m-%dT%H:%M:%OSZ"),
                    TRUE ~ as.Date(CalvingDate, format="%Y-%m-%dT%H:%M:%OSZ"))
                  ,
                  CalvingMonth = format(CalvingDate, "%m"),
                  CalvingSeason = as.factor(
                        case_when(
                          CalvingMonth == "01" ~ "Winter",
                          CalvingMonth == "02" ~ "Winter",
                          CalvingMonth == "03" ~ "Spring",
                          CalvingMonth == "04" ~ "Spring",
                          CalvingMonth == "05" ~ "Spring",
                          CalvingMonth == "06" ~ "Summer",
                          CalvingMonth == "07" ~ "Summer",
                          CalvingMonth == "08" ~ "Summer",
                          CalvingMonth == "09" ~ "Fall",
                          CalvingMonth == "10" ~ "Fall",
                          CalvingMonth == "11" ~ "Fall",
                          CalvingMonth == "12" ~ "Winter",
                          TRUE ~ "Other"
                          )),
                  WeeksInMilk = floor(as.numeric(DaysInMilk)/7),
                  WeeksInMilk = case_when(
                    between(DaysInMilk, -1, 1) ~ "Calving",
                    WeeksInMilk == -3 ~ "WeekPre3",
                    WeeksInMilk == -2 ~ "WeekPre2",
                    WeeksInMilk == -1 ~ "WeekPre1",
                    WeeksInMilk == 0 ~ "WeekPost1",
                    WeeksInMilk == 1 ~ "WeekPost2",
                    WeeksInMilk == 2 ~ "WeekPost3"),
                  AnimalEartag = as.character(AnimalEartag),
                  HerdIdentifier = as.character(HerdIdentifier),
                  AnimalIdentifier = as.character(AnimalIdentifier)
                  ) %>%
    dplyr::group_by(
      HerdIdentifier, 
      AnimalIdentifier,
      AnimalEartag,
      CalvingDate,
      CalvingSeason, 
      Parity,
      LactationNumber,
      SensorType,
      WeeksInMilk
      ) %>% 
  dplyr::summarise(
    AvgSensorValue = mean(SensorValue, na.rm = TRUE)
  ) %>%
  tidyr::unite("SensorWeek", SensorType, WeeksInMilk, sep = "CowAvg") %>%
  tidyr::spread(key = SensorWeek, value = AvgSensorValue, convert = TRUE) %>%
  drop_na()
```

    ## `summarise()` regrouping output by 'HerdIdentifier', 'AnimalIdentifier', 'AnimalEartag', 'CalvingDate', 'CalvingSeason', 'Parity', 'LactationNumber', 'SensorType' (override with `.groups` argument)

### Herd features

``` r
PredictionHerdData <- AllData %>%
    dplyr::filter(
      between(DaysInMilk, -21, 20) &
      stringr::str_detect(SensorType, 'Rumination time|Eating time|Lying time|Standup')
      ) %>%
    dplyr::mutate(
                  PrePostPartum = case_when(
                    between(DaysInMilk, -1, 1) ~ "Calving",
                    DaysInMilk < 0 ~ "Precalving",
                    DaysInMilk > 0 ~ "Postcalving"),
                  AnimalEartag = as.character(AnimalEartag),
                  HerdIdentifier = as.character(HerdIdentifier),
                  AnimalIdentifier = as.character(AnimalIdentifier)
                  ) %>%
    dplyr::group_by(
      HerdIdentifier, 
      SensorType,
      PrePostPartum
      ) %>% 
  dplyr::summarise(
    AvgSensorValue = mean(SensorValue, na.rm = TRUE)
  ) %>%
  tidyr::unite("SensorWeek", SensorType, PrePostPartum, sep = "HerdAvg") %>%
  tidyr::spread(key = SensorWeek, value = AvgSensorValue, convert = TRUE) %>%
  drop_na()
```

    ## `summarise()` regrouping output by 'HerdIdentifier', 'SensorType' (override with `.groups` argument)

## Locomotion data

``` r
LocomotionData <- 
  read_csv(
    "../Data/LocomotionAndBCSData.csv", 
    col_types = cols(
      DaysInMilk = col_integer(), 
      HerdIdentifier = col_character(), 
      LOCO = col_double(), 
      LactationNumber = col_integer())) %>%
  dplyr::mutate(
    CalvingDate = as.Date(CalvingTime, format="%Y-%m-%d"),
    AnimalEartag = as.character(AnimalNumber),
    HerdIdentifier = as.character(HerdIdentifier),
    AnimalIdentifier = as.character(AnimalIdentifier),
    LocomotionScore = as.factor(case_when(
                    LOCO <= 3 ~ "1-3",
                    TRUE ~ "4-5"))
    ) %>% # Only select the necesarry columns
  dplyr::mutate_at(vars(anoCols), anonymizer::anonymize, .char = anoSalt) %>%
  dplyr::select(
    AnimalIdentifier,
    HerdIdentifier,
    LactationNumber,
    LocomotionScore,
    ObservationPeriod,
    ObservationDate
  )
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(anoCols)` instead of `anoCols` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
LocomotionDataWeek8 <- LocomotionData %>% dplyr::filter(ObservationPeriod == "WEEK 8")
```

## BCS data

``` r
BCSData <- 
  read_csv(
    "../Data/LocomotionAndBCSData.csv", 
    col_types = cols(
      DaysInMilk = col_integer(), 
      HerdIdentifier = col_character(), 
      LactationNumber = col_integer())) %>%
  dplyr::mutate(
    CalvingDate = as.Date(CalvingTime, format="%Y-%m-%d"),
    AnimalEartag = as.character(AnimalNumber),
    HerdIdentifier = as.character(HerdIdentifier),
    AnimalIdentifier = as.character(AnimalIdentifier),
    ObservationPeriod = case_when(
                    ObservationPeriod == "BEGIN DROOGSTAND" ~ "BCSEarlyDryPeriod",
                    ObservationPeriod == "EIND DROOGSTAND" ~ "BCSEndDryPeriod",
                    ObservationPeriod == "WEEK 4" ~ "BCSWeek4",
                    TRUE ~ "BCSWeek8"),
    ) %>% # Only select the necesarry columns
  dplyr::mutate_at(vars(anoCols), anonymizer::anonymize, .char = anoSalt) %>%
  dplyr::group_by(
    AnimalIdentifier,
    HerdIdentifier,
    LactationNumber,
    ObservationPeriod
  ) %>%
  dplyr::summarise(BCS = mean(BCS, na.rm = TRUE)) %>%
  tidyr::spread(key = ObservationPeriod, value = BCS, convert = TRUE) %>%
  dplyr::mutate(
    BCSEarlyDryPeriodCat = factor(case_when(
      BCSEarlyDryPeriod < 3.0 ~ "< 3.0",
      BCSEarlyDryPeriod > 3.5 ~ "> 3.5",
      TRUE ~ "3.0 to 3.5"),
      levels= c("< 3.0", "3.0 to 3.5", "> 3.5")
    ),
    BCSEndDryPeriodCat = factor(case_when(
      BCSEndDryPeriod < 3.0 ~ "< 3.0",
      BCSEndDryPeriod > 3.5 ~ "> 3.5",
      TRUE ~ "3.0 to 3.5"),
      levels= c("< 3.0", "3.0 to 3.5", "> 3.5")
    ),
    BCSWeek4Cat = factor(case_when(
      BCSWeek4 < 2.5 ~ "< 2.5",
      BCSWeek4 > 2.75 ~ "> 2.75",
      TRUE ~ "2.5 to 2.75"),
      levels= c("< 2.5", "2.5 to 2.75", "> 2.75")
    ),   
    BCSWeek8Cat = factor(case_when(
      BCSWeek8 < 2.5 ~ "< 2.5",
      BCSWeek8 > 2.75 ~ "> 2.75",
      TRUE ~ "2.5 to 2.75"),
      levels= c("< 2.5", "2.5 to 2.75", "> 2.75")
    ),    
    BCSDeltaDryPeriod = BCSEndDryPeriod - BCSEarlyDryPeriod,
    BCSDeltaTransition =  BCSWeek4 - BCSEndDryPeriod,
    BCSDeltaPostPartum =  BCSWeek8 - BCSWeek4,
    BCSDeltaDryCat = factor(case_when(
      BCSDeltaDryPeriod > 0.25 ~ "Increased more than 0.25",
      BCSDeltaDryPeriod < 0.00 ~ "Decreased",
      TRUE ~ "Stable 0.00 to 0.25"),
      levels= c("Decreased", "Stable 0.00 to 0.25", "Increased more than 0.25")
    ),
    BCSDeltaTransitionCat = factor(case_when(
      BCSDeltaTransition > -0.50 ~ "Decreased less than 0.50",
      BCSDeltaTransition < -0.75 ~ "Decreased more than 0.75",
      TRUE ~ "Decreased between 0.75 to 0.50"),
      levels= c("Decreased more than 0.75", "Decreased between 0.75 to 0.50", "Decreased less than 0.50")
    ),
    BCSDeltaPostPartumCat = factor(case_when(
      BCSDeltaPostPartum > -0.50 ~ "Decreased less than 0.50",
      BCSDeltaPostPartum < -0.75 ~ "Decreased more than 0.75",
      TRUE ~ "Decreased between 0.75 to 0.50"),
      levels= c("Decreased more than 0.75", "Decreased between 0.75 to 0.50", "Decreased less than 0.50")
    )
  )
```

    ## `summarise()` regrouping output by 'AnimalIdentifier', 'HerdIdentifier', 'LactationNumber' (override with `.groups` argument)

## Combined data

``` r
Week8Data <- PredictionData %>%
  inner_join(
    LocomotionDataWeek8,
    by = c("AnimalIdentifier", 
           "HerdIdentifier", 
           "LactationNumber")
    ) %>%
  inner_join(
    BCSData %>% dplyr::select(
      AnimalIdentifier, 
      HerdIdentifier, 
      LactationNumber,
      BCSEarlyDryPeriod,
      BCSEndDryPeriod,
      BCSWeek4,
      BCSWeek8,
      BCSDeltaDryPeriod,
      BCSDeltaTransition,
      BCSDeltaPostPartum
    ),
    by = c("AnimalIdentifier", 
           "HerdIdentifier", 
           "LactationNumber")
    ) %>%
  inner_join(
    PredictionHerdData,
    by = c("HerdIdentifier")
  ) %>%
  drop_na() 
names(Week8Data) <- gsub("[^[:alnum:]]","",names(Week8Data))
```

# Random forest prediction locomotion score week 8

## Create test and training data

``` r
cols <- colnames(Week8Data)
SensorCols <- cols[grepl("Avg", cols)]
nonSensorCols <- c("Parity", "CalvingSeason", "BCSEarlyDryPeriod", "BCSEndDryPeriod", "BCSWeek4", "BCSWeek8" ,"BCSDeltaDryPeriod", "BCSDeltaTransition", "BCSDeltaPostPartum")
predictorCols <- c(SensorCols, nonSensorCols)
outcomeCols <- c("LocomotionScore")
allCols <- c(SensorCols, nonSensorCols, outcomeCols)

FullWeek8Data <- Week8Data %>% 
  ungroup() %>% 
  select(one_of(allCols))

table(Week8Data$LocomotionScore)
```

    ## 
    ## 1-3 4-5 
    ## 317 165

``` r
BalancedWeek8Data <- upSample(
  x = FullWeek8Data[, !names(FullWeek8Data) %in% c("LocomotionScore")],
  y = FullWeek8Data$LocomotionScore)
table(BalancedWeek8Data$Class)
```

    ## 
    ## 1-3 4-5 
    ## 317 317

``` r
trainIndex <- createDataPartition(BalancedWeek8Data$Class, p = .7, 
                                  list = FALSE, 
                                  times = 1)

TrainingFullData <- BalancedWeek8Data[ trainIndex,]
table(TrainingFullData$Class)
```

    ## 
    ## 1-3 4-5 
    ## 222 222

``` r
TestFullData  <- BalancedWeek8Data[-trainIndex,]
table(TestFullData$Class)
```

    ## 
    ## 1-3 4-5 
    ##  95  95

## Model

``` r
rfFormula <- reformulate(
  termlabels = c(SensorCols, nonSensorCols),
  response = "Class"
  )

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3
                     )

#Number randomely variable selected is mtry
mtry <- 3
tunegrid <- expand.grid(.mtry=mtry)

rfClassifier <- train(
  rfFormula, 
  data=as.data.frame(TrainingFullData),
  preProcess=c("scale"),
  method='rf', 
  metric='Accuracy',
  tuneGrid=tunegrid, 
  trControl=control)
print(rfClassifier)
```

    ## Random Forest 
    ## 
    ## 444 samples
    ##  49 predictor
    ##   2 classes: '1-3', '4-5' 
    ## 
    ## Pre-processing: scaled (52) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 399, 400, 400, 399, 400, 400, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.7845374  0.5692276
    ## 
    ## Tuning parameter 'mtry' was held constant at a value of 3

### Variable importance

``` r
varImp(rfClassifier)$importance %>% rownames_to_column() %>% arrange(-Overall)
```

    ##                                   rowname    Overall
    ## 1         EatingtimemindayCowAvgWeekPost3 100.000000
    ## 2          EatingtimemindayCowAvgWeekPre3  88.466304
    ## 3         EatingtimemindayCowAvgWeekPost2  84.927915
    ## 4         EatingtimemindayCowAvgWeekPost1  84.523919
    ## 5               StandupndayCowAvgWeekPre3  84.128970
    ## 6          EatingtimemindayCowAvgWeekPre2  80.422285
    ## 7           EatingtimemindayCowAvgCalving  74.192061
    ## 8     RuminationtimemindayCowAvgWeekPost1  72.420172
    ## 9               StandupndayCowAvgWeekPre1  71.172617
    ## 10             StandupndayCowAvgWeekPost2  67.827155
    ## 11             StandupndayCowAvgWeekPost3  66.811319
    ## 12    RuminationtimemindayCowAvgWeekPost2  66.660349
    ## 13              StandupndayCowAvgWeekPre2  66.232782
    ## 14           LyingtimemindayCowAvgCalving  64.501017
    ## 15     RuminationtimemindayCowAvgWeekPre2  62.540346
    ## 16     RuminationtimemindayCowAvgWeekPre1  62.029974
    ## 17     RuminationtimemindayCowAvgWeekPre3  60.632538
    ## 18         LyingtimemindayCowAvgWeekPost1  60.418009
    ## 19         EatingtimemindayCowAvgWeekPre1  60.354984
    ## 20    RuminationtimemindayCowAvgWeekPost3  58.253653
    ## 21         LyingtimemindayCowAvgWeekPost3  57.977642
    ## 22          LyingtimemindayCowAvgWeekPre1  57.458777
    ## 23      RuminationtimemindayCowAvgCalving  55.795813
    ## 24          LyingtimemindayCowAvgWeekPre2  55.687840
    ## 25          LyingtimemindayCowAvgWeekPre3  54.963458
    ## 26         LyingtimemindayCowAvgWeekPost2  54.555406
    ## 27             StandupndayCowAvgWeekPost1  52.886414
    ## 28               StandupndayCowAvgCalving  46.131191
    ## 29                        BCSEndDryPeriod  45.406009
    ## 30                               BCSWeek8  43.082031
    ## 31     EatingtimemindayHerdAvgPostcalving  41.525458
    ## 32                               BCSWeek4  37.097006
    ## 33                     BCSDeltaTransition  37.037170
    ## 34                      BCSDeltaDryPeriod  33.963309
    ## 35                      BCSEarlyDryPeriod  33.218351
    ## 36                     BCSDeltaPostPartum  28.957149
    ## 37              StandupndayHerdAvgCalving  21.805455
    ## 38                               Parity>3  19.384108
    ## 39      EatingtimemindayHerdAvgPrecalving  17.937795
    ## 40          StandupndayHerdAvgPostcalving  17.638126
    ## 41           StandupndayHerdAvgPrecalving  17.504211
    ## 42       LyingtimemindayHerdAvgPrecalving  15.994027
    ## 43      LyingtimemindayHerdAvgPostcalving  15.112488
    ## 44 RuminationtimemindayHerdAvgPostcalving  14.774399
    ## 45          LyingtimemindayHerdAvgCalving  14.108764
    ## 46         EatingtimemindayHerdAvgCalving  13.979158
    ## 47     RuminationtimemindayHerdAvgCalving  13.778272
    ## 48                    CalvingSeasonWinter  12.550945
    ## 49  RuminationtimemindayHerdAvgPrecalving  10.273251
    ## 50                                Parity3   4.362892
    ## 51                    CalvingSeasonSummer   4.297008
    ## 52                    CalvingSeasonSpring   0.000000

``` r
predictionsForROCCurve <- predict(rfClassifier,TestFullData)
library(caret)
confusionMatrix(predictionsForROCCurve, TestFullData$Class)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction 1-3 4-5
    ##        1-3  73  18
    ##        4-5  22  77
    ##                                           
    ##                Accuracy : 0.7895          
    ##                  95% CI : (0.7246, 0.8451)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.5789          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.6353          
    ##                                           
    ##             Sensitivity : 0.7684          
    ##             Specificity : 0.8105          
    ##          Pos Pred Value : 0.8022          
    ##          Neg Pred Value : 0.7778          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.3842          
    ##    Detection Prevalence : 0.4789          
    ##       Balanced Accuracy : 0.7895          
    ##                                           
    ##        'Positive' Class : 1-3             
    ## 

### Validation

``` r
# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
if (!require("ROCR")) {
  install.packages("ROCR", dependencies = TRUE)
  library(ROCR)
}

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
predictionsForROCCurve <- predict(rfClassifier,TestFullData,type="prob")

# Use pretty colours:
prettyColours <- c("#F8766D","#00BA38")

# Specify the different classes 
classes <- levels(TestFullData$Class)

# For each class
for (i in 1:2)
{
 # Define which observations belong to class[i]
 TrueValues <- ifelse(TestFullData[,50]==classes[i],1,0)
 
 # Assess the performance of classifier for class[i]
 Pred <- prediction(predictionsForROCCurve[,i],TrueValues)
 Perf <- performance(Pred, "tpr", "fpr")
 if (i==1)
 {
     plot(Perf,main="ROC Curve",col=prettyColours[i]) 
 }
 else
 {
     plot(Perf,main="ROC Curve",col=prettyColours[i],add=TRUE) 
 }
 
 # Calculate the AUC and print it to screen
 auc.perf <- performance(Pred, measure = "auc")
 print(auc.perf@y.values)
}
```

![](PredictionLocoWeek8_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

    ## [[1]]
    ## [1] 0.8959003
    ## 
    ## [[1]]
    ## [1] 0.8959003

# Plots

## Eating time

``` r
pdp::grid.arrange(
  top = "Effect of eating time on odds ratio for locomotion score 1-3 at week 8 in lactation",
  rfClassifier %>%
    partial(pred.var = "EatingtimemindayCowAvgWeekPre3") %>%
    ggplot(., aes(x=EatingtimemindayCowAvgWeekPre3, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("Odds ratio") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -3") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "EatingtimemindayCowAvgWeekPre2") %>%
    ggplot(., aes(x=EatingtimemindayCowAvgWeekPre2, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -2") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "EatingtimemindayCowAvgWeekPre1") %>%
    ggplot(., aes(x=EatingtimemindayCowAvgWeekPre1, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -1") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "EatingtimemindayCowAvgWeekPost1") %>%
    ggplot(., aes(x=EatingtimemindayCowAvgWeekPost1, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("Odds ratio") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 1") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "EatingtimemindayCowAvgWeekPost2") %>%
    ggplot(., aes(x=EatingtimemindayCowAvgWeekPost2, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 2") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "EatingtimemindayCowAvgWeekPost3") %>%
    ggplot(., aes(x=EatingtimemindayCowAvgWeekPost3, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 3") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  nrow = 2
  )
```

![](PredictionLocoWeek8_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Rumination time

``` r
pdp::grid.arrange(
  top = "Effect of rumination time on odds ratio for locomotion score 1-3 at week 8 in lactation",
  rfClassifier %>%
    partial(pred.var = "RuminationtimemindayCowAvgWeekPre3") %>%
    ggplot(., aes(x=RuminationtimemindayCowAvgWeekPre3, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("Odds ratio") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -3") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "RuminationtimemindayCowAvgWeekPre2") %>%
    ggplot(., aes(x=RuminationtimemindayCowAvgWeekPre2, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -2") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "RuminationtimemindayCowAvgWeekPre1") %>%
    ggplot(., aes(x=RuminationtimemindayCowAvgWeekPre1, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -1") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "RuminationtimemindayCowAvgWeekPost1") %>%
    ggplot(., aes(x=RuminationtimemindayCowAvgWeekPost1, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("Odds ratio") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 1") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "RuminationtimemindayCowAvgWeekPost2") %>%
    ggplot(., aes(x=RuminationtimemindayCowAvgWeekPost2, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 2") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "RuminationtimemindayCowAvgWeekPost3") %>%
    ggplot(., aes(x=RuminationtimemindayCowAvgWeekPost3, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("min/day") +
    xlim(0, 750) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 3") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  nrow = 2
  )
```

![](PredictionLocoWeek8_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Standups

``` r
pdp::grid.arrange(
  top = "Effect of number of standups on odds ratio for locomotion score 1-3 at week 8 in lactation",
  rfClassifier %>%
    partial(pred.var = "StandupndayCowAvgWeekPre3") %>%
    ggplot(., aes(x=StandupndayCowAvgWeekPre3, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("Odds ratio") +
    xlab("n/day") +
    xlim(0, 30) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -3") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "StandupndayCowAvgWeekPre2") %>%
    ggplot(., aes(x=StandupndayCowAvgWeekPre2, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("n/day") +
    xlim(0, 30) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -2") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "StandupndayCowAvgWeekPre1") %>%
    ggplot(., aes(x=StandupndayCowAvgWeekPre1, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("n/day") +
    xlim(0, 30) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week -1") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "StandupndayCowAvgWeekPost1") %>%
    ggplot(., aes(x=StandupndayCowAvgWeekPost1, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("Odds ratio") +
    xlab("n/day") +
    xlim(0, 30) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 1") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "StandupndayCowAvgWeekPost2") %>%
    ggplot(., aes(x=StandupndayCowAvgWeekPost2, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("n/day") +
    xlim(0, 30) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 2") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  rfClassifier %>%
    partial(pred.var = "StandupndayCowAvgWeekPost3") %>%
    ggplot(., aes(x=StandupndayCowAvgWeekPost3, y=exp(yhat))) +
    geom_line(stat="identity") + 
    geom_hline(yintercept=1) +
    geom_smooth( se = FALSE) + 
    ylab("") +
    xlab("n/day") +
    xlim(0, 30) +
    coord_cartesian(ylim=c(0.5,1.5)) + 
    theme_light() +
    labs(subtitle = "Week 3") +
    theme(plot.subtitle = element_text(hjust = 0.5)),
  nrow = 2
  )
```

![](PredictionLocoWeek8_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

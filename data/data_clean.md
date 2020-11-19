cleaned\_data
================

``` r
convert = function(x){
       if (x<10) {
  x = str_c(0,x) 
       }
          convert_fips_to_names(FIPs = as.character(x)) 
  }
cleaned = 
        read_csv("raw.csv") %>% 
        janitor::clean_names() %>% 
        mutate(states = as.character(map(.x = statefip,~convert(.x))), ##output is a list, convert to  characters
               metropolitan_status = recode(metro, 
                                            "0" = "mixed of metropolitian status", 
                                            "1" = "Not in metropolitan area",
                                            "2" = "in metroplitan and central city",
                                            "3" = "in metroplitian but not in central city",
                                            "4" = "in metropolitian but mixed of central city"),
                                number_of_children = nchild,
                gradeatt = recode(gradeatt,  ## get who is currently in his/her freshman year
                                  "7" = "1",
                                  .default = "0"),
               educ = recode(educ,    ## get who has at least finished the freshman year
                             "07" = "1",
                             "08" = "1",
                             "09" = "1",
                             "10" = "1",
                             "11" = "1",
                             .default = "0"),
               college_attendance_status = recode((as.numeric(gradeatt) + as.numeric(educ)), 
                                                 "1" = "Yes",
                                                 "2" = "Yes",
                                                 "0" = "No"),
               highest_degree = recode(educd, 
                                       "114" = "master or preofessional degree",
                                       "115" = "master or professional degree",
                                       "116" = "doctor degree",
                                       "101" = "Bachelor's degree",
                                       .default = "undergraduate student or below"),
               school_type = recode(schltype,
                                    "3" = "Private school",
                                    "2" = "Public school",
                                    "1" = "Not enrolled"),
               anually_family_income = replace(ftotinc, ftotinc == "9999999", NA)) %>% 
        select(year, age, states, metropolitan_status, number_of_children, college_attendance_status,
               highest_degree, school_type, anually_family_income)
```

    ## Parsed with column specification:
    ## cols(
    ##   YEAR = col_double(),
    ##   STATEFIP = col_double(),
    ##   METRO = col_double(),
    ##   NCHILD = col_double(),
    ##   SEX = col_double(),
    ##   AGE = col_double(),
    ##   RACE = col_double(),
    ##   RACED = col_double(),
    ##   EDUC = col_double(),
    ##   EDUCD = col_double(),
    ##   GRADEATT = col_double(),
    ##   GRADEATTD = col_double(),
    ##   SCHLTYPE = col_double(),
    ##   FTOTINC = col_double()
    ## )

``` r
write_csv(cleaned,"./cleaned.csv")
```

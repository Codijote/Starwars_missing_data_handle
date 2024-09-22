library(tidyverse)
library(beepr)
library(naniar)

root_data <- starwars

# Visualizing missing data
vis_miss(root_data)
# From the missing datapoints only height and mass are numeric variables, others cannot be estimated nor
# taken in average because of their nature, like sex, birth_year or homeworld.

# Dimesions of root_data at the begining of the checks
dim(root_data)
# [1] 87 14

# Removing all NA results on too much data lost, 60% of the data will be lost.
squimed_data <- na.omit(root_data)
dim(squimed_data)
# [1] 29 14
rm(squimed_data)

# from the variables with missing data only height and mass are numerical
# and prone to prediction work.
# All missing height rows also lack the mass and 32% of the rows lack the mass
# data.
# Mass and height cannot be entirely completed with the average because sometimes they
# belong to different species or sexes in every specie.

root_data |> filter(is.na(height))
# A tibble: 6 × 14
# name        height  mass hair_color skin_color eye_color birth_year sex   gender homeworld species films vehicles starships
# <chr>        <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr>  <chr>     <chr>   <lis> <list>   <list>   
#   1 Arvel Cryn…     NA    NA brown      fair       brown             NA male  mascu… NA        Human   <chr> <chr>    <chr [1]>
#   2 Finn            NA    NA black      dark       dark              NA male  mascu… NA        Human   <chr> <chr>    <chr [0]>
#   3 Rey             NA    NA brown      light      hazel             NA fema… femin… NA        Human   <chr> <chr>    <chr [0]>
#   4 Poe Dameron     NA    NA brown      light      brown             NA male  mascu… NA        Human   <chr> <chr>    <chr [1]>
#   5 BB8             NA    NA none       none       black             NA none  mascu… NA        Droid   <chr> <chr>    <chr [0]>
#   6 Captain Ph…     NA    NA none       none       unknown           NA fema… femin… NA        Human   <chr> <chr>    <chr [0]>

# By simple eye counting we find that from 6 the missing height values 5 are humans, 3 male and 2 female; and the 6th is a droid.
# So average for human males, human females and droids can be applied to the missing heights
# The segmentation between species and sex are a challenge at the time to substitute specific means

# *

Droid_means <- starwars |> filter(species == "Droid") |> summarize(height = mean(height, na.rm = TRUE), mass = mean(mass, na.rm = TRUE))
# A tibble: 1 × 2
# height  mass
# <dbl> <dbl>
#   1   131.  69.8
Human_male_means <- starwars |> filter(species == "Human" & sex == "male") |> summarize(height = mean(height, na.rm = TRUE), mass = mean(mass, na.rm = TRUE))
# A tibble: 1 × 2
# height  mass
# <dbl> <dbl>
#   1   182.  85.7
Human_female_means <- starwars |> filter(species == "Human" & sex == "female") |> summarize(height = mean(height, na.rm = TRUE), mass = mean(mass, na.rm = TRUE))
# A tibble: 1 × 2
# height  mass
# <dbl> <dbl>
#   1   164.  56.3

# Imputing the corresponding values by category

completed_data <- root_data
# filter(species == "Droid") |> 
completed_data <- completed_data |> 
  mutate(height = case_when(is.na(height) & species == "Droid" ~ Droid_means$height,
                            TRUE ~ height)) |> 
  mutate(mass = case_when(is.na(mass) & species == "Droid" ~ Droid_means$mass,
                          TRUE ~ mass)) |> 
  mutate(hair_color = case_when(is.na(hair_color) & species == "Droid"~ "none",
                                TRUE ~ hair_color))

# A tibble: 6 × 14
# name   height  mass hair_color skin_color  eye_color birth_year sex   gender    homeworld species films     vehicles  starships
# <chr>   <dbl> <dbl> <chr>      <chr>       <chr>          <dbl> <chr> <chr>     <chr>     <chr>   <list>    <list>    <list>   
#   1 C-3PO    167   75   none       gold        yellow           112 none  masculine Tatooine  Droid   <chr [6]> <chr [0]> <chr [0]>
#   2 R2-D2     96   32   none       white, blue red               33 none  masculine Naboo     Droid   <chr [7]> <chr [0]> <chr [0]>
#   3 R5-D4     97   32   none       white, red  red               NA none  masculine Tatooine  Droid   <chr [1]> <chr [0]> <chr [0]>
#   4 IG-88    200  140   none       metal       red               15 none  masculine NA        Droid   <chr [1]> <chr [0]> <chr [0]>
#   5 R4-P17    96   69.8 none       silver, red red, blue         NA none  feminine  NA        Droid   <chr [2]> <chr [0]> <chr [0]>
#   6 BB8      131.  69.8 none       none        black             NA none  masculine NA        Droid   <chr [1]> <chr [0]> <chr [0]>

completed_data <- completed_data |> 
  mutate(height = case_when(is.na(height) & species == "Human" & sex == "male" ~ Human_male_means$height,
                            TRUE ~ height)) |> 
  mutate(mass = case_when(is.na(mass) & species == "Human" & sex == "male" ~ Human_male_means$mass,
                          TRUE ~ mass))

completed_data <- completed_data |> 
  mutate(height = case_when(is.na(height) & species == "Human" & sex == "female" ~ Human_female_means$height,
                            TRUE ~ height)) |> 
  mutate(mass = case_when(is.na(mass) & species == "Human" & sex == "female" ~ Human_female_means$mass,
                          TRUE ~ mass))

# At this point other 11 datapoints are missing mass but they are frequently for only 1 element of the specie or minority.

completed_data |> filter(is.na(mass))
# A tibble: 11 × 14
# name        height  mass hair_color skin_color  eye_color birth_year sex    gender    homeworld  species   films     vehicles  starships
# <chr>        <dbl> <dbl> <chr>      <chr>       <chr>          <dbl> <chr>  <chr>     <chr>      <chr>     <list>    <list>    <list>   
#   1 Rugor Nass     206    NA none       green       orange            NA male   masculine Naboo      Gungan    <chr [1]> <chr [0]> <chr [0]>
#   2 Watto          137    NA black      blue, grey  yellow            NA male   masculine Toydaria   Toydarian <chr [2]> <chr [0]> <chr [0]>
#   3 Bib Fortuna    180    NA none       pale        pink              NA male   masculine Ryloth     Twi'lek   <chr [1]> <chr [0]> <chr [0]>
#  4 Gasgano        122    NA none       white, blue black             NA male   masculine Troiken    Xexto     <chr [1]> <chr [0]> <chr [0]>
#  5 Eeth Koth      171    NA black      brown       brown             NA male   masculine Iridonia   Zabrak    <chr [2]> <chr [0]> <chr [0]>
#  6 Saesee Tiin    188    NA none       pale        orange            NA male   masculine Iktotch    Iktotchi  <chr [2]> <chr [0]> <chr [0]>
#  7 Yarael Poof    264    NA none       white       yellow            NA male   masculine Quermia    Quermian  <chr [1]> <chr [0]> <chr [0]>
#  8 Mas Amedda     196    NA none       blue        blue              NA male   masculine Champala   Chagrian  <chr [2]> <chr [0]> <chr [0]>
#  9 Cordé          157    NA brown      light       brown             NA NA     NA        Naboo      NA        <chr [1]> <chr [0]> <chr [0]>
# 10 Taun We        213    NA none       grey        black             NA female feminine  Kamino     Kaminoan  <chr [1]> <chr [0]> <chr [0]>
# 11 San Hill       191    NA none       grey        gold              NA male   masculine Muunilinst Muun      <chr [1]> <chr [0]> <chr [0]>
# 

df <- completed_data
rm(completed_data, Droid_means, Human_female_means, Human_male_means, root_data)

vis_miss(df)
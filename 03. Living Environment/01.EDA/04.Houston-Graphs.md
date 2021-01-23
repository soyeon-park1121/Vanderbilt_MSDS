Harris Final Graphs
================

# General Data Cleaning

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(ggiraphExtra)
library(ggrepel)
options(scipen = 999)
df <- read_csv("/Users/Nelson/Desktop/R/eda20-team5-project/data.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   geoid = col_character(),
    ##   msaname15 = col_character(),
    ##   countyfips = col_character(),
    ##   statefips = col_character(),
    ##   stateusps = col_character()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
# Separate the column "msaname15" into "city", "state", "size" and "no_mean"
df <- df %>%
  separate(msaname15, c("city", "state", "size", "no_mean"), sep = " ")
```

    ## Warning: Expected 4 pieces. Additional pieces discarded in 61334 rows [337, 338,
    ## 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354,
    ## 355, 356, ...].

``` r
df<- df %>%
  mutate(num_edu_center = exp(ED_PRXECE)) %>%
  mutate(num_good_edu_center = exp(ED_PRXHQECE)) %>%
  mutate(num_waste_dump_site = exp(HE_SUPRFND)) %>%
  mutate(population = exp(HE_RSEI)) %>%
  select(-ED_PRXECE, -ED_PRXHQECE, -HE_SUPRFND, -HE_RSEI)
df <- df %>%
  rename(id = `_id`) %>%
  rename(geo_id = geoid) %>%
  rename(metro_areas = in100) %>%
  rename(area_code = msaid15) %>%
  rename(county_code = countyfips) %>%
  rename(num_under_18 = pop) %>%
  rename(AP_students = ED_APENR) %>%
  rename(college_deg = ED_ATTAIN) %>%
  rename(num_college_enrolled = ED_COLLEGE) %>%
  rename(preschoolers = ED_ECENROL) %>%
  rename(hs_grads = ED_HSGRAD) %>%
  rename(third_g_math = ED_MATH) %>%
  rename(third_g_read = ED_READING) %>%
  rename(elementary_school_poverty = ED_SCHPOV) %>%
  rename(firstsecond_y_teachers =ED_TEACHXP) %>%
  rename(supermarket_nearby = HE_FOOD) %>%
  rename(green_spaces = HE_GREEN) %>%
  rename(days_temp_above90 = HE_HEAT) %>%
  rename(phealth_insurance = HE_HLTHINS) %>%
  rename(mean_ozone_amount = HE_OZONE) %>%
  rename(mean_microparticle = HE_PM25) %>%
  rename(housing_vacancy = HE_VACANCY) %>%
  rename(walkability = HE_WALK) %>%
  rename(below100_poverty = SE_POVRATE) %>%
  rename(public_assistance = SE_PUBLIC) %>%
  rename(home_ownership = SE_HOME) %>%
  rename(perc_over15_high_skill = SE_OCC) %>%
  rename(median_income=SE_MHE) %>%
  rename(employment_rate = SE_EMPRAT) %>%
  rename(commute_over1hour = SE_JOBPROX) %>%
  rename(single_parents = SE_SINGLE)
df <- df %>% select(-state, -no_mean, -statefips, -num_waste_dump_site)
extra_NA<- df %>%
  select_if(function(x) any(is.na(x))) %>%
  summarise_each(funs(sum(is.na(.))))
```

    ## Warning: `summarise_each_()` is deprecated as of dplyr 0.7.0.
    ## Please use `across()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
NA_subset <- df[rowSums(is.na(df)) > 0, ]
# Delete some rows having more NAs than meaningful values.
df <- df %>% filter((geo_id != 47155980100) & (geo_id != 47145980100) & (geo_id != 47031980100) & (geo_id != 47029980100) & (geo_id != 47009980200) & (geo_id != 47009980100) & (geo_id != 47001980100) & (geo_id != 47037980100) & (geo_id != 47037980200) & (geo_id != 47037013000))
df <- df %>%
  #NYC
  mutate(county_code = replace(county_code, county_code == '36061', 'Manhattan')) %>%
  mutate(county_code = replace(county_code, county_code == '36047', 'Brooklyn')) %>%
  mutate(county_code = replace(county_code, county_code == '36081', 'Queens')) %>%
  mutate(county_code = replace(county_code, county_code == '36005', 'Bronx')) %>%
  mutate(county_code = replace(county_code, county_code == '36085', 'Staten Island'))%>%
  #Philly
  mutate(county_code = replace(county_code, county_code == '42017', 'Bucks')) %>%
  mutate(county_code = replace(county_code, county_code == '42029', 'Chester')) %>%
  mutate(county_code = replace(county_code, county_code == '42045', 'Delaware')) %>%
  mutate(county_code = replace(county_code, county_code == '42091', 'Montgomery')) %>%
  mutate(county_code = replace(county_code, county_code == '42101', 'Philadelphia')) %>%
  #Harris
  mutate(county_code = replace(county_code, county_code == "48201", "Harris")) %>%
  mutate(county_code = replace(county_code, county_code == "48157", "Fort Bend")) %>%
  mutate(county_code = replace(county_code, county_code == "48339", "Montgomery")) %>%
  mutate(county_code = replace(county_code, county_code == "48167", "Galveston")) %>%
  mutate(county_code = replace(county_code, county_code == "48039", "Brazoria")) %>%
  mutate(county_code = replace(county_code, county_code == "48291", "Liberty")) %>%
  mutate(county_code = replace(county_code, county_code == "48473", "Waller")) %>%
  mutate(county_code = replace(county_code, county_code == "48071", "Chambers"))%>%
  #LA
  mutate(county_code = replace(county_code,county_code == "06111", "Ventura")) %>%
  mutate(county_code = replace(county_code, county_code == "06059","Orange"))%>%
  mutate(county_code = replace(county_code, county_code == "06071","San Bernardino"))%>%
  mutate(county_code = replace(county_code,county_code == "06037", "Los Angeles"))%>%
  mutate(county_code = replace(county_code,county_code == "06029", "Kern"))%>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
```

    ## Warning: Problem with `mutate()` input `city`.
    ## ℹ argument is not numeric or logical: returning NA
    ## ℹ Input `city` is `(structure(function (..., .x = ..1, .y = ..2, . = ..1) ...`.

    ## Warning in mean.default(.x, na.rm = TRUE): argument is not numeric or logical:
    ## returning NA

    ## Warning: Problem with `mutate()` input `size`.
    ## ℹ argument is not numeric or logical: returning NA
    ## ℹ Input `size` is `(structure(function (..., .x = ..1, .y = ..2, . = ..1) ...`.

    ## Warning in mean.default(.x, na.rm = TRUE): argument is not numeric or logical:
    ## returning NA

``` r
Cities <-
  df %>% filter(
    county_code == 'Manhattan'|
    county_code == 'Brooklyn'|
    county_code == 'Queens'|
    county_code == 'Bronx'|
    county_code == 'Staten Island'|
    county_code == 'Bucks'|
    county_code == 'Chester'|
    county_code == 'Delaware'|
    county_code == 'Montgomery'|
    county_code == 'Philadelphia'|
    county_code == 'Harris'|
    county_code == 'Montgomery'|
    county_code == 'Fort Bend'|
    county_code == 'Liberty'|
    county_code == 'Waller'|
    county_code == 'Ventura'|
    county_code == 'Orange'|
    county_code == 'San Bernardino'|
    county_code == 'Los Angeles'|
    county_code == 'Kern'
  ) %>% na.omit()
income <-Cities %>% select(median_income) %>% summarise(mean = mean(median_income))
text_color <- c('red','black','black','black','black','black','black')
```

# Harris Data Cleaning

``` r
df_tx <- Cities %>% filter(stateusps == 'TX') %>% filter(county_code == "Harris" | county_code == "Fort Bend"| county_code == "Montgomery"| county_code == "Liberty"| county_code == "Waller")

df_tx <- df_tx %>% mutate(group = ifelse(county_code == "Harris", "city", "suburb")) %>%
  select(group, everything())

df_tx$county_code = factor(df_tx$county_code, levels = c("Harris", "Fort Bend", "Montgomery", "Waller", "Liberty"))

df_tx_2010 <- df_tx %>% filter(year == "2010")
df_tx_2015 <- df_tx %>% filter(year == "2015")
```

# Economics

## College Degree vs Income

``` r
library(tidyverse)
library(viridis)
library(factoextra)
library(data.table)
library(ggiraphExtra)
library(ggrepel)
options(scipen = 999)
```

``` r
df_tx_2015 %>% mutate(City_Suburb = ifelse(county_code == 'Harris','City','Suburb')) %>% 
  ggplot( aes(x = college_deg, y = median_income))+
  geom_point(aes(color = City_Suburb), alpha = .5, size = 2)+
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")})+
  scale_x_continuous(limits = c(0,100),labels = function(x){paste0(x, "%")})+
  labs(
    title = 'Median Income vs. College Degree in Houston',
    x = 'College Degree',
    y = 'Median Income',
    color = 'City or Suburb'
  )+
  theme_classic()
```

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## High Skill vs Income

``` r
df_tx_2015 %>% mutate(City_Suburb = ifelse(county_code == 'Harris','City','Suburb')) %>% 
  ggplot( aes(x = perc_over15_high_skill, y = median_income))+
  geom_point(aes(color = City_Suburb), alpha = .5, size = 2)+
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")})+
  scale_x_continuous(labels = function(x){paste0(x, "%")}, limits  = c(0,100))+
  labs(
    title = 'Median Income vs. Skilled Labor in Houston',
    x = 'Skilled Labor',
    y = 'Median Income',
    color = 'City or Suburb'
  )+
  theme_classic()
```

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Homeownership

``` r
o.color = c("red", "black", "black", "black", "black")
ggplot(df_tx_2015, aes(x = reorder(county_code, home_ownership), y = median_income)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .35, aes(color = home_ownership)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_viridis(name = "% Home Ownership",
                      breaks = seq(0,100,25),
                      labels = function(x){paste0(x, "%")}) +
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")}) +
    labs(
    x = "County",
    y = "Median Income",
    title = "Home Ownership in Houston and its Suburbs",
    subtitle = "Vs. Houston Area Median Income"
  ) +
  geom_hline(yintercept = median(df_tx_2015$median_income), color = "red", linetype = "dashed")+
  theme_bw()+
  theme(axis.text.x = element_text(colour = o.color))
```

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## Results may be unexpected or may change in future versions of ggplot2.

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Income

``` r
income_tx <- df_tx_2015 %>%
  group_by(county_code)%>%
  summarise(mean_inc = mean(median_income))%>%
  mutate(deviation= mean_inc - income$mean)%>%
  arrange(desc(mean_inc))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
i.color = c("black", "black", "red", "black", "black")
ggplot(income_tx, aes(x = reorder(county_code, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household Income in Houston",
    subtitle="Vs. Houston Area Average Median Income, $64K",
    x = "County",
    y = "Amount Difference"
  )+
  theme_bw()+
  theme(axis.text.y = element_text(colour = i.color)) +
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")}, limits=c(-25000,30000)) 
```

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## Results may be unexpected or may change in future versions of ggplot2.

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Education

## Third Grade Reading Scores

``` r
df_tx_2015 %>% 
  mutate(Area = ifelse(county_code == "Harris", " City", "Suburb")) %>% 
  ggplot(aes(x = county_code, y = third_g_math + third_g_read, fill = Area, alpha = Area)) +
  geom_boxplot(outlier.alpha=0) +
  geom_hline(data = df_tx_2015, aes(yintercept = mean(third_g_read, na.rm = TRUE) + mean(third_g_math, na.rm = TRUE)), col = "red",linetype='dotted') + 
  scale_fill_manual(values = c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  theme_classic() +
  labs(title = "2015 Houston 3rd Grades Reading and Math Scores", 
       subtitle = "Vs. National Average", 
       x = "County", 
       y = "3rd Grades Reading & Math scores") +
  scale_y_continuous(limits=c(0,1000))
```

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## High School Graduation Rates

``` r
data <-Cities %>% 
  drop_na(hs_grads)
#average grad rates in 2015
grad_rates_tx <- data %>%
  filter(county_code %in% c("Harris", "Fort Bend", "Montgomery", "Waller", "Liberty"))%>%
  group_by(county_code,year)%>%
  summarise(mean_grad = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_code' (override with `.groups` argument)

``` r
grad_rates_good <- grad_rates_tx %>% filter(county_code %in% c("Harris", "Fort Bend", "Waller", "Liberty"))
grad_rates_bad <-  grad_rates_tx %>% filter(county_code %in% c("Montgomery"))
```

``` r
grad_color = c("black", "red", "black", "black", "black")
ggplot() + 
  geom_point(data = grad_rates_tx,aes(x = mean_grad, y = county_code, color = factor(year)), size = 4, alpha = .8)+
  geom_line(data = grad_rates_good, aes(x = mean_grad, y = county_code), arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
  geom_line(data = grad_rates_bad, aes(x = mean_grad, y = county_code), arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
  labs(title = "Change in Houston High School Graduation Rate", 
       x = "Percent Graduated",
       y = "County",
       color = 'Year')+
  theme_classic()+
  theme(panel.grid.major.x=element_line(), axis.text.y = element_text(colour = grad_color))+
  scale_x_continuous(
            breaks=seq(50, 100, 5),
            limits = c(65,100),
            labels = function(x){paste0(x*1, '%')}
            )
```

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## Results may be unexpected or may change in future versions of ggplot2.

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## AP Classes

``` r
ap_tx<- df_tx_2015 %>%
  filter(county_code == "Harris" & median_income < income$mean)
tx_wealthy <- df_tx %>%
  filter(county_code== "Harris" & median_income > income$mean)

ap_sub_tx<- df_tx_2015 %>%
  filter(county_code !="Harris" & median_income < income$mean)
sub_tx_wealthy <- df_tx %>%
  filter(county_code != "Harris" & median_income > income$mean)
```

``` r
colors <- c("More than $66K" = "light blue", "Less than $66K" = "pink")

ggplot()+
  geom_density(aes(AP_students, fill= "Less than $66K"), alpha = .2, data = ap_tx)+
  geom_density(aes(AP_students, fill= "More than $66K"), alpha = .2,  data = tx_wealthy)+
  labs(
    title="AP Courses vs. Income",
    subtitle="Houston",
    x="Ratio of Upper Level Students in AP Courses",
    y="Frequency",
    color = colors,
    fill = 'AP Students'
  )+
  scale_x_continuous(limits = c(0,2), breaks = seq(0,2,.25))+
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,1))+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students, fill="Less than $66K"), alpha = .2, data = ap_sub_tx)+
  geom_density(aes(AP_students, fill="More than $66K"), alpha = .2,  data = sub_tx_wealthy)+
  labs(
    title="AP Courses vs. Income",
    subtitle="Houston Suburbs",
    x="Ratio of Upper Level Students in AP Courses",
    y="Frequency",
    color = colors,
    fill = 'AP Students'
  )+
  scale_x_continuous(limits = c(0,2), breaks = seq(0,2,.25))+
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8,1))+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

# Livability

``` r
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_tx_2015_live <- df_tx_2015 %>% filter(!is.na(supermarket_nearby)) %>%
  filter(!is.na(green_spaces)) %>%
  filter(!is.na(walkability)) %>%
  mutate(norm_super = normalize(supermarket_nearby)) %>% 
  mutate(norm_green = normalize(green_spaces)) %>% 
  mutate(norm_walk = normalize(walkability)) %>% 
  group_by(county_code) %>%
  summarise(norm_super = mean(norm_super), norm_green = mean(norm_green), norm_walk = mean(norm_walk)) %>% 
  mutate(total =norm_super+norm_green+norm_walk)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
df_tx_2015_live <- as.data.frame(df_tx_2015_live)
county_code <- df_tx_2015_live %>% pull(county_code)
norm_super <- df_tx_2015_live %>% pull(norm_super)
norm_green <- df_tx_2015_live %>% pull(norm_green)
norm_walk <- df_tx_2015_live %>% pull(norm_walk)

df2 <- rbind(
        data.frame(county_code, "count" = norm_super, "Factor"="Near Supermarket"),
        data.frame(county_code, "count" = norm_green, "Factor"="Green Space"),
        data.frame(county_code, "count" = norm_walk, "Factor" = "Walkability")
)

ggplot(df2, aes(x = df2$county_code, y = df2$count, fill = df2$Factor))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set3") +
  theme_bw()+
  labs(title = "Houston Index of Residential Environment", subtitle = "Livability = Green Space Access + Near Supermarket + Walkability", y = "Index of Residential Environment", x = "County", fill = 'Factor')+
  theme(axis.text.x = element_text(color = text_color)) +
  scale_y_continuous(limits=c(0,1.5))
```

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## Results may be unexpected or may change in future versions of ggplot2.

    ## Warning: Use of `df2$county_code` is discouraged. Use `county_code` instead.

    ## Warning: Use of `df2$count` is discouraged. Use `count` instead.

    ## Warning: Use of `df2$Factor` is discouraged. Use `Factor` instead.

![](Texas-Final-Graphics_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

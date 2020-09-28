Lab 05 - Data Wrangling
================
Robin Du

# Learning goals

  - Use the `merge()` function to join two datasets.
  - Deal with missings and impute data.
  - Identify relevant observations using `quantile()`.
  - Practice your GitHub skills.

# Lab description

For this lab we will be, again, dealing with the meteorological dataset
downloaded from the NOAA, the `met`. In this case, we will use
`data.table` to answer some questions regarding the `met` dataset, while
at the same time practice your Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the
    data) in your computer, and create a folder for this project, for
    example, “PM566-labs”

2.  In that folder, save [this
    template](https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository, hopefully of
    the same name that this folder has, i.e., “PM566-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir PM566-labs
cd PM566-labs

# Step 2
wget https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd 
mv 05-lab.Rmd README.md

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/PM566-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("PM566-labs")
setwd("PM566-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/05-lab.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/PM566-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

<!-- end list -->

``` r
library(data.table)
met <- fread("C:/PM566/Lab5/met_all.gz") 
```

2.  Load the met data from
    <https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz>,
    and also the station data. For the later, you can use the code we
    used during lecture to pre-process the stations data:

<!-- end list -->

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

<!-- end list -->

``` r
met <- merge(
  x = met, y = stations, 
  by.x = "USAFID", by.y = "USAF", 
  all.x = TRUE, all.y = FALSE
  )

# Print out a sample of the data
met[1:5, .(USAFID, WBAN, STATE)]
```

    ##    USAFID  WBAN STATE
    ## 1: 690150 93121    CA
    ## 2: 690150 93121    CA
    ## 3: 690150 93121    CA
    ## 4: 690150 93121    CA
    ## 5: 690150 93121    CA

## Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent continental US using the `quantile()` function. Do these three
coincide?

``` r
# Obtaining averages per station
met_stations <- met[, .(
    temp      = mean(temp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE),
    wind.sp   = mean(wind.sp, na.rm = TRUE)
  ), by = .(USAFID, STATE)]

# Computing the median
met_stations[, temp50      := quantile(temp, probs = .5, na.rm = TRUE)]
met_stations[, atm.press50 := quantile(atm.press, probs = .5, na.rm = TRUE)]
met_stations[, wind.sp50   := quantile(wind.sp, probs = .5, na.rm = TRUE)]

# Filtering the data
met_stations[which.min(abs(temp - temp50))]
```

    ##    USAFID STATE     temp atm.press  wind.sp   temp50 atm.press50 wind.sp50
    ## 1: 720458    KY 23.68173       NaN 1.209682 23.68406    1014.691  2.461838

``` r
met_stations[which.min(abs(atm.press - atm.press50))]
```

    ##    USAFID STATE     temp atm.press  wind.sp   temp50 atm.press50 wind.sp50
    ## 1: 722238    AL 26.13978  1014.691 1.472656 23.68406    1014.691  2.461838

``` r
met_stations[which.min(abs(wind.sp - wind.sp50))]
```

    ##    USAFID STATE     temp atm.press  wind.sp   temp50 atm.press50 wind.sp50
    ## 1: 720929    WI 17.43278       NaN 2.461838 23.68406    1014.691  2.461838

  - No, these three do not coincide.

Knit the document, commit your changes, and Save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
# Computing the median
met_stations[, temp50s      := quantile(temp, probs = .5, na.rm = TRUE), by = STATE]
met_stations[, atm.press50s := quantile(atm.press, probs = .5, na.rm = TRUE), by = STATE]
met_stations[, wind.sp50s   := quantile(wind.sp, probs = .5, na.rm = TRUE), by = STATE]

# Temperature
met_stations[, tempdif  := which.min(abs(temp - temp50s)), by = STATE]
met_stations[, recordid := 1:.N, by = STATE]
met_temp <- met_stations[recordid == tempdif, .(USAFID, temp, temp50s, STATE)]

# Atmospheric pressure
met_stations[, atm.pressdif  := which.min(abs(atm.press - atm.press50s)), by = STATE]
met_stations[, recordid := 1:.N, by = STATE]
met_stations[recordid == atm.pressdif, .(USAFID, atm.press, atm.press50s, STATE)]
```

    ##     USAFID atm.press atm.press50s STATE
    ##  1: 722029  1015.335     1015.335    FL
    ##  2: 722085  1015.298     1015.281    SC
    ##  3: 722093  1014.906     1014.927    MI
    ##  4: 722181  1015.208     1015.208    GA
    ##  5: 722269  1014.926     1014.959    AL
    ##  6: 722320  1014.593     1014.593    LA
    ##  7: 722340  1014.842     1014.836    MS
    ##  8: 722479  1012.464     1012.460    TX
    ##  9: 722745  1010.144     1010.144    AZ
    ## 10: 722899  1012.557     1012.557    CA
    ## 11: 723109  1015.420     1015.420    NC
    ## 12: 723300  1014.522     1014.522    MO
    ## 13: 723346  1015.144     1015.144    TN
    ## 14: 723436  1014.591     1014.591    AR
    ## 15: 723537  1012.567     1012.567    OK
    ## 16: 723600  1012.404     1012.525    NM
    ## 17: 724037  1015.158     1015.158    VA
    ## 18: 724040  1014.824     1014.824    MD
    ## 19: 724075  1014.825     1014.825    NJ
    ## 20: 724120  1015.757     1015.762    WV
    ## 21: 724180  1015.046     1015.046    DE
    ## 22: 724237  1015.236     1015.245    KY
    ## 23: 724286  1015.351     1015.351    OH
    ## 24: 724373  1015.063     1015.063    IN
    ## 25: 724586  1013.389     1013.389    KS
    ## 26: 724660  1013.334     1013.334    CO
    ## 27: 724860  1011.947     1012.204    NV
    ## 28: 725040  1014.810     1014.810    CT
    ## 29: 725053  1014.887     1014.887    NY
    ## 30: 725064  1014.721     1014.751    MA
    ## 31: 725070  1014.837     1014.728    RI
    ## 32: 725109  1015.474     1015.435    PA
    ## 33: 725440  1014.760     1014.760    IL
    ## 34: 725461  1014.957     1014.964    IA
    ## 35: 725555  1014.345     1014.332    NE
    ## 36: 725686  1013.157     1013.157    WY
    ## 37: 725755  1012.243     1011.972    UT
    ## 38: 725784  1012.908     1012.855    ID
    ## 39: 725895  1014.726     1015.269    OR
    ## 40: 726114  1014.792     1014.792    VT
    ## 41: 726155  1014.689     1014.689    NH
    ## 42: 726196  1014.323     1014.399    ME
    ## 43: 726425  1014.893     1014.893    WI
    ## 44: 726545  1014.497     1014.398    SD
    ## 45: 726559  1015.042     1015.042    MN
    ## 46: 726777  1014.299     1014.185    MT
    ##     USAFID atm.press atm.press50s STATE

``` r
# Wind speed
met_stations[, wind.spdif  := which.min(abs(wind.sp - wind.sp50s)), by = STATE]
met_stations[, recordid := 1:.N, by = STATE]
met_stations[recordid == wind.spdif, .(USAFID, wind.sp, wind.sp50s, STATE)]
```

    ##     USAFID  wind.sp wind.sp50s STATE
    ##  1: 720254 1.268571   1.268571    WA
    ##  2: 720328 1.617823   1.633487    WV
    ##  3: 720386 2.617071   2.617071    MN
    ##  4: 720422 3.679474   3.680613    KS
    ##  5: 720492 1.408247   1.408247    VT
    ##  6: 720532 3.098777   3.098777    CO
    ##  7: 720602 1.616549   1.696119    SC
    ##  8: 720858 3.972789   3.956459    ND
    ##  9: 720951 1.493666   1.495596    GA
    ## 10: 720971 3.873392   3.873392    WY
    ## 11: 721031 1.513550   1.576035    TN
    ## 12: 722029 2.699017   2.705069    FL
    ## 13: 722076 2.244115   2.237622    IL
    ## 14: 722165 1.599550   1.636392    MS
    ## 15: 722202 3.404683   3.413737    TX
    ## 16: 722218 1.883499   1.883499    MD
    ## 17: 722275 1.662132   1.662132    AL
    ## 18: 722486 1.592840   1.592840    LA
    ## 19: 722676 3.776083   3.776083    NM
    ## 20: 722740 3.125322   3.074359    AZ
    ## 21: 722899 2.561738   2.565445    CA
    ## 22: 723010 1.641749   1.627306    NC
    ## 23: 723415 1.875302   1.938625    AR
    ## 24: 723545 3.852697   3.852697    OK
    ## 25: 723860 2.968539   3.035050    NV
    ## 26: 724006 1.650539   1.653032    VA
    ## 27: 724090 2.148606   2.148606    NJ
    ## 28: 724180 2.752929   2.752929    DE
    ## 29: 724303 2.606462   2.554397    OH
    ## 30: 724350 1.930836   1.895486    KY
    ## 31: 724373 2.347673   2.344333    IN
    ## 32: 724458 2.459746   2.453547    MO
    ## 33: 724700 3.180628   3.145427    UT
    ## 34: 725016 2.376050   2.304075    NY
    ## 35: 725079 2.583469   2.583469    RI
    ## 36: 725087 2.126514   2.101801    CT
    ## 37: 725088 2.773018   2.710944    MA
    ## 38: 725103 1.784167   1.784167    PA
    ## 39: 725464 2.679227   2.680875    IA
    ## 40: 725624 3.192539   3.192539    NE
    ## 41: 725867 2.702517   2.568944    ID
    ## 42: 725975 2.080792   2.011436    OR
    ## 43: 726056 1.556907   1.563826    NH
    ## 44: 726077 2.337241   2.237210    ME
    ## 45: 726284 2.273423   2.273423    MI
    ## 46: 726504 2.053283   2.053283    WI
    ## 47: 726519 3.665638   3.665638    SD
    ## 48: 726770 4.151737   4.151737    MT
    ##     USAFID  wind.sp wind.sp50s STATE

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
met_stations <- unique(met[, .(USAFID, STATE, lon, lat)])

met_stations[, .SD[1], by = USAFID]
```

    ##       USAFID STATE      lon    lat
    ##    1: 690150    CA -116.166 34.300
    ##    2: 720110    TX  -98.662 30.784
    ##    3: 720113    MI  -83.178 42.543
    ##    4: 720120    SC  -80.697 32.224
    ##    5: 720137    IL  -88.419 41.425
    ##   ---                             
    ## 1591: 726777    MT -104.250 46.358
    ## 1592: 726797    MT -111.160 45.788
    ## 1593: 726798    MT -110.440 45.698
    ## 1594: 726810    ID -116.233 43.567
    ## 1595: 726813    ID -116.633 43.650

``` r
met_stations[, lat_mid := quantile(lat, probs = .5, na.rm = TRUE), by = STATE]
met_stations[, lon_mid := quantile(lon, probs = .5, na.rm = TRUE), by = STATE]

# Looking at the euclidean distances
met_stations[, distance := sqrt((lat - lat_mid)^2 + (lon - lon_mid)^2)]
met_stations[, minrecord := which.min(distance), by = STATE]
met_stations[, n := 1:.N, by = STATE]
met_location <- met_stations[n == minrecord, .(USAFID, STATE, lon, lat)]
```

``` r
all_stations <- met[, .(USAFID, lat, lon, STATE)][, .SD[1], by = "USAFID"]

# Recovering lon and lat from the original dataset
met_temp <- merge(
  x = met_temp,
  y = all_stations,
  by = "USAFID",
  all.x = TRUE, all.y = FALSE
)
```

``` r
library(leaflet)

# Combining datasets
dat1 <- met_location[, .(lon, lat)]
dat1[, type := "Center of the state"]

dat2 <- met_temp[, .(lon, lat)]
dat2[, type := "Center of the temperature"]

dat <- rbind(dat1, dat2)

# Copy and paste from previous lab
rh_pal <- colorFactor(c('blue','red'),
                      domain = as.factor(dat$type))
leaflet(dat) %>%
  addProviderTiles("OpenStreetMap") %>%
  addCircles(lng = ~lon, lat = ~lat, color = ~rh_pal(type),
             opacity=1, fillOpacity = 1, radius = 500)
```

<!--html_preserve-->

<div id="htmlwidget-c80233b35bd04d7a6ee1" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-c80233b35bd04d7a6ee1">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[39,47.104,37.578,30.558,37.4,47.451,32.64,35.582,28.821,32.3,32.311,31.15,35.003,33.467,33.967,36.009,35.259,35.415,36.317,39.175,39.133,40.033,40.477,38.707,38.068,39.6,41.509,41.876,41.597,40.821,42.209,40.616,40.648,43.322,41.991,40.967,40.219,43.5,42.367,43.205,44.533,44.534,39.05,44.359,44.381,44.859,43.067,45.808,45.417,46.683,42.574,39,41.384,30.46,34.717,38.35,48.784,30.033,29.445,35.438,44.523,35.211,33.355,38.533,37.033,31.183,28.85,38.586,32.167,32.867,35.867,36.009,36.744,40.033,39.674,40.82,40.412,39.135,39.909,38.051,42.571,41.91,41.733,41.333,41.914,40.717,42.4,40.219,44.533,43.344,43.626,43.156,43.683,45.604,44.339,46.358],[-80.274,-122.287,-84.77,-92.099,-77.517,-99.151,-83.592,-79.101,-81.81,-86.408,-90.076,-97.717,-105.662,-111.733,-80.8,-86.52,-93.093,-97.387,-119.628,-76.668,-75.467,-74.353,-88.916,-93.176,-97.275,-116.01,-72.829,-71.021,-71.412,-76.864,-75.98,-83.063,-86.152,-84.688,-93.619,-98.317,-111.723,-114.3,-122.867,-71.503,-69.667,-72.614,-105.516,-89.837,-100.285,-94.382,-108.45,-108.543,-123.817,-122.983,-84.811,-80.274,-72.506,-87.877,-79.95,-93.683,-97.632,-85.533,-90.261,-94.803,-114.215,-91.738,-84.567,-76.033,-85.95,-90.471,-96.917,-77.711,-110.883,-117.133,-78.783,-86.52,-108.229,-74.35,-75.606,-82.518,-86.937,-96.679,-105.117,-117.09,-77.713,-70.729,-71.433,-75.717,-88.246,-99,-96.383,-111.723,-69.667,-72.518,-72.305,-90.678,-93.367,-103.546,-105.541,-104.25],500,null,null,{"interactive":true,"className":"","stroke":true,"color":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"weight":5,"opacity":1,"fill":true,"fillColor":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"fillOpacity":1},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[28.821,48.784],"lng":[-123.817,-69.667]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

  - low: temp \< 20
  - Mid: temp \>= 20 and temp \< 25
  - High: temp \>= 25

Once you are done with that, you can compute the following:

  - Number of entries (records),
  - Number of NA entries,
  - Number of stations,
  - Number of states included, and
  - Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub. If
you’d like, you can take this time to include the link of [the issue
of the week](https://github.com/USCbiostats/PM566/issues/23) so that you
let us know when you are done, e.g.,

``` bash
git commit -a -m "Finalizing lab 5 https://github.com/USCbiostats/PM566/issues/23"
```

Market Segmentation
================

``` r
rm(list=ls())
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Loading required package: ggstance

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Loading required package: Matrix

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median,
    ##     prop.test, quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

``` r
library(dplyr)

raw.data = read.csv('social_marketing.csv', header=TRUE)

summary(raw.data)
```

    ##          X           chatter       current_events      travel      
    ##  123pxkyqj:   1   Min.   : 0.000   Min.   :0.000   Min.   : 0.000  
    ##  12grikctu:   1   1st Qu.: 2.000   1st Qu.:1.000   1st Qu.: 0.000  
    ##  12klxic7j:   1   Median : 3.000   Median :1.000   Median : 1.000  
    ##  12t4msroj:   1   Mean   : 4.399   Mean   :1.526   Mean   : 1.585  
    ##  12yam59l3:   1   3rd Qu.: 6.000   3rd Qu.:2.000   3rd Qu.: 2.000  
    ##  132y8f6aj:   1   Max.   :26.000   Max.   :8.000   Max.   :26.000  
    ##  (Other)  :7876                                                    
    ##  photo_sharing    uncategorized      tv_film      sports_fandom   
    ##  Min.   : 0.000   Min.   :0.000   Min.   : 0.00   Min.   : 0.000  
    ##  1st Qu.: 1.000   1st Qu.:0.000   1st Qu.: 0.00   1st Qu.: 0.000  
    ##  Median : 2.000   Median :1.000   Median : 1.00   Median : 1.000  
    ##  Mean   : 2.697   Mean   :0.813   Mean   : 1.07   Mean   : 1.594  
    ##  3rd Qu.: 4.000   3rd Qu.:1.000   3rd Qu.: 1.00   3rd Qu.: 2.000  
    ##  Max.   :21.000   Max.   :9.000   Max.   :17.00   Max.   :20.000  
    ##                                                                   
    ##     politics           food            family        home_and_garden 
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.0000   Min.   :0.0000  
    ##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.0000   1st Qu.:0.0000  
    ##  Median : 1.000   Median : 1.000   Median : 1.0000   Median :0.0000  
    ##  Mean   : 1.789   Mean   : 1.397   Mean   : 0.8639   Mean   :0.5207  
    ##  3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.: 1.0000   3rd Qu.:1.0000  
    ##  Max.   :37.000   Max.   :16.000   Max.   :10.0000   Max.   :5.0000  
    ##                                                                      
    ##      music              news        online_gaming       shopping     
    ##  Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
    ##  Median : 0.0000   Median : 0.000   Median : 0.000   Median : 1.000  
    ##  Mean   : 0.6793   Mean   : 1.206   Mean   : 1.209   Mean   : 1.389  
    ##  3rd Qu.: 1.0000   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 2.000  
    ##  Max.   :13.0000   Max.   :20.000   Max.   :27.000   Max.   :12.000  
    ##                                                                      
    ##  health_nutrition  college_uni     sports_playing      cooking      
    ##  Min.   : 0.000   Min.   : 0.000   Min.   :0.0000   Min.   : 0.000  
    ##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.: 0.000  
    ##  Median : 1.000   Median : 1.000   Median :0.0000   Median : 1.000  
    ##  Mean   : 2.567   Mean   : 1.549   Mean   :0.6392   Mean   : 1.998  
    ##  3rd Qu.: 3.000   3rd Qu.: 2.000   3rd Qu.:1.0000   3rd Qu.: 2.000  
    ##  Max.   :41.000   Max.   :30.000   Max.   :8.0000   Max.   :33.000  
    ##                                                                     
    ##       eco           computers          business         outdoors      
    ##  Min.   :0.0000   Min.   : 0.0000   Min.   :0.0000   Min.   : 0.0000  
    ##  1st Qu.:0.0000   1st Qu.: 0.0000   1st Qu.:0.0000   1st Qu.: 0.0000  
    ##  Median :0.0000   Median : 0.0000   Median :0.0000   Median : 0.0000  
    ##  Mean   :0.5123   Mean   : 0.6491   Mean   :0.4232   Mean   : 0.7827  
    ##  3rd Qu.:1.0000   3rd Qu.: 1.0000   3rd Qu.:1.0000   3rd Qu.: 1.0000  
    ##  Max.   :6.0000   Max.   :16.0000   Max.   :6.0000   Max.   :12.0000  
    ##                                                                       
    ##      crafts         automotive           art             religion     
    ##  Min.   :0.0000   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000  
    ##  1st Qu.:0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.000  
    ##  Median :0.0000   Median : 0.0000   Median : 0.0000   Median : 0.000  
    ##  Mean   :0.5159   Mean   : 0.8299   Mean   : 0.7248   Mean   : 1.095  
    ##  3rd Qu.:1.0000   3rd Qu.: 1.0000   3rd Qu.: 1.0000   3rd Qu.: 1.000  
    ##  Max.   :7.0000   Max.   :13.0000   Max.   :18.0000   Max.   :20.000  
    ##                                                                       
    ##      beauty          parenting           dating            school       
    ##  Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000  
    ##  1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000  
    ##  Median : 0.0000   Median : 0.0000   Median : 0.0000   Median : 0.0000  
    ##  Mean   : 0.7052   Mean   : 0.9213   Mean   : 0.7109   Mean   : 0.7677  
    ##  3rd Qu.: 1.0000   3rd Qu.: 1.0000   3rd Qu.: 1.0000   3rd Qu.: 1.0000  
    ##  Max.   :14.0000   Max.   :14.0000   Max.   :24.0000   Max.   :11.0000  
    ##                                                                         
    ##  personal_fitness    fashion        small_business        spam        
    ##  Min.   : 0.000   Min.   : 0.0000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.: 0.000   1st Qu.: 0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
    ##  Median : 0.000   Median : 0.0000   Median :0.0000   Median :0.00000  
    ##  Mean   : 1.462   Mean   : 0.9966   Mean   :0.3363   Mean   :0.00647  
    ##  3rd Qu.: 2.000   3rd Qu.: 1.0000   3rd Qu.:1.0000   3rd Qu.:0.00000  
    ##  Max.   :19.000   Max.   :18.0000   Max.   :6.0000   Max.   :2.00000  
    ##                                                                       
    ##      adult        
    ##  Min.   : 0.0000  
    ##  1st Qu.: 0.0000  
    ##  Median : 0.0000  
    ##  Mean   : 0.4033  
    ##  3rd Qu.: 0.0000  
    ##  Max.   :26.0000  
    ## 

``` r
raw.data <- distinct(raw.data)
raw.data[is.na(raw.data)] <- 0
```

``` r
head(raw.data)
```

    ##           X chatter current_events travel photo_sharing uncategorized
    ## 1 hmjoe4g3k       2              0      2             2             2
    ## 2 clk1m5w8s       3              3      2             1             1
    ## 3 jcsovtak3       6              3      4             3             1
    ## 4 3oeb4hiln       1              5      2             2             0
    ## 5 fd75x1vgk       5              2      0             6             1
    ## 6 h6nvj91yp       6              4      2             7             0
    ##   tv_film sports_fandom politics food family home_and_garden music news
    ## 1       1             1        0    4      1               2     0    0
    ## 2       1             4        1    2      2               1     0    0
    ## 3       5             0        2    1      1               1     1    1
    ## 4       1             0        1    0      1               0     0    0
    ## 5       0             0        2    0      1               0     0    0
    ## 6       1             1        0    2      1               1     1    0
    ##   online_gaming shopping health_nutrition college_uni sports_playing
    ## 1             0        1               17           0              2
    ## 2             0        0                0           0              1
    ## 3             0        2                0           0              0
    ## 4             0        0                0           1              0
    ## 5             3        2                0           4              0
    ## 6             0        5                0           0              0
    ##   cooking eco computers business outdoors crafts automotive art religion
    ## 1       5   1         1        0        2      1          0   0        1
    ## 2       0   0         0        1        0      2          0   0        0
    ## 3       2   1         0        0        0      2          0   8        0
    ## 4       0   0         0        1        0      3          0   2        0
    ## 5       1   0         1        0        1      0          0   0        0
    ## 6       0   0         1        1        0      0          1   0        0
    ##   beauty parenting dating school personal_fitness fashion small_business
    ## 1      0         1      1      0               11       0              0
    ## 2      0         0      1      4                0       0              0
    ## 3      1         0      1      0                0       1              0
    ## 4      1         0      0      0                0       0              0
    ## 5      0         0      0      0                0       0              1
    ## 6      0         0      0      0                0       0              0
    ##   spam adult
    ## 1    0     0
    ## 2    0     0
    ## 3    0     0
    ## 4    0     0
    ## 5    0     0
    ## 6    0     0

``` r
# Center and scale the data
X = raw.data[,-2]
X = X[,-5]
X = X[,-1]
#X
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")
```

Elbow method to find a value of k. Since k = 10 seems like an elbow, we
are going to take k = 10 to create clusters.

``` r
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=50)
  cluster_k$tot.withinss
}
```

    ## Warning: did not converge in 10 iterations
    
    ## Warning: did not converge in 10 iterations
    
    ## Warning: did not converge in 10 iterations
    
    ## Warning: did not converge in 10 iterations
    
    ## Warning: did not converge in 10 iterations

``` r
plot(k_grid, SSE_grid)
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Run k-means with 10 clusters and 25 starts
clust1 = kmeans(X, 10, nstart=25)

# What are the clusters?
clust1$center  # not super helpful
```

    ##    current_events      travel photo_sharing      tv_film sports_fandom
    ## 1      0.37082201 -0.20874717    1.16393164 -0.126745905    -0.2101211
    ## 2     -0.08106613 -0.04267660   -0.01646228  0.095326209    -0.1266487
    ## 3      0.30490339  0.22608904   -0.05886756  2.780408584    -0.1152744
    ## 4      0.10695128  3.23569564   -0.11277465 -0.067845058    -0.2136210
    ## 5      0.10988087 -0.09813918   -0.08116542 -0.095409247     2.0782988
    ## 6      0.18073936 -0.06258883    1.25478537 -0.154102573    -0.2051438
    ## 7      0.07009668 -0.18841407   -0.20447614 -0.007645991     0.6677047
    ## 8     -0.20084867 -0.21888279   -0.42083617 -0.220995123    -0.3224679
    ## 9     -0.01082130 -0.15671938   -0.08396781 -0.145869520    -0.2012404
    ## 10     0.27684711  0.28872703   -0.09071828 -0.116191009     0.1406567
    ##       politics        food      family home_and_garden         music
    ## 1  -0.15514307 -0.31111498 -0.04676972      0.15320798  1.360905e-01
    ## 2  -0.17563345 -0.09679282  0.20136205      0.06834696 -4.558890e-02
    ## 3  -0.08990204  0.13949048 -0.12518758      0.32054648  1.038927e+00
    ## 4   3.08819300  0.15371770 -0.10116116      0.04649105 -4.414151e-02
    ## 5  -0.22631717  1.84148800  1.51713078      0.16499274  4.375758e-02
    ## 6  -0.13049890 -0.20895488  0.03927668      0.14748087  5.407528e-01
    ## 7   1.21986287 -0.16546991  0.23209424      0.15020726 -8.680212e-02
    ## 8  -0.29967074 -0.36270666 -0.30518848     -0.20443565 -2.288449e-01
    ## 9  -0.19899244  0.44590794 -0.08006987      0.14373238 -6.915049e-05
    ## 10  0.15052740  0.04049422 -0.05999555      0.23510191  1.418264e-02
    ##             news online_gaming     shopping health_nutrition college_uni
    ## 1  -0.2788837583   -0.17112731  1.478696655      -0.28227853 -0.10742678
    ## 2  -0.1933037537    3.57175617 -0.135180882      -0.18130040  3.27353844
    ## 3   0.0096511100   -0.17003728  0.076937344      -0.15541216  0.37134035
    ## 4   1.1263907669   -0.16889548 -0.064200463      -0.16939138 -0.04478930
    ## 5  -0.1110972157   -0.07661410 -0.001255721      -0.14540284 -0.12639829
    ## 6  -0.0735988923   -0.02314104  0.204719459      -0.05912092 -0.01813325
    ## 7   2.6430813586   -0.12914740 -0.154114346      -0.25427982 -0.19204785
    ## 8  -0.3098205759   -0.23238776 -0.397735514      -0.31397710 -0.25675200
    ## 9  -0.0787475730   -0.11780607 -0.028207110       2.19022016 -0.21258648
    ## 10 -0.0006901999    0.08935906 -0.237822640       0.05086059  0.12732753
    ##    sports_playing     cooking          eco   computers    business
    ## 1     -0.08134027 -0.23621096  0.339339112 -0.02765760  0.38398008
    ## 2      2.11739791 -0.12373614 -0.064842621 -0.08270086 -0.10437622
    ## 3      0.11551124 -0.13693759  0.124244298 -0.15058657  0.36352221
    ## 4      0.03202883 -0.18755257  0.165163040  2.89840374  0.56042253
    ## 5      0.11285195 -0.09481309  0.195459551  0.09365098  0.11721326
    ## 6      0.19868032  2.76816671  0.006502437  0.06441720  0.22999239
    ## 7     -0.08834545 -0.24669524 -0.084837993 -0.19476736 -0.11210726
    ## 8     -0.25550756 -0.33128790 -0.282172786 -0.25815456 -0.25139072
    ## 9     -0.01292797  0.40203877  0.535912931 -0.08389355  0.06742258
    ## 10    -0.11129036 -0.05898219  0.447999475  0.29753290 -0.34600901
    ##       outdoors      crafts  automotive          art    religion
    ## 1  -0.29024423  0.08129551  0.06353701 -0.218600658 -0.25961406
    ## 2  -0.14273994  0.02321121  0.05725025  0.268010799 -0.18947730
    ## 3  -0.08702067  0.77799812 -0.22420093  2.688972381  0.01885619
    ## 4  -0.03458032  0.20301368 -0.13320504 -0.163024723  0.11199530
    ## 5  -0.07004825  0.69680699  0.12238906 -0.014688654  2.28570404
    ## 6   0.02645310  0.08405787  0.02316114  0.002643756 -0.12534281
    ## 7   0.30161324 -0.16891614  2.60214809 -0.169066894 -0.18700363
    ## 8  -0.32374021 -0.29019382 -0.30836484 -0.235144913 -0.30359527
    ## 9   1.69445205  0.06956351 -0.16532250 -0.072595434 -0.16762389
    ## 10  0.29780310  0.21793373  0.12453564  0.331675372  0.12070179
    ##           beauty   parenting        dating      school personal_fitness
    ## 1  -0.2140298275 -0.20260784  0.1388467869  0.03815298      -0.23030751
    ## 2  -0.2289601107 -0.13926299 -0.0003107033 -0.21708164      -0.18863321
    ## 3  -0.0003285909 -0.18225289 -0.0563517401 -0.01959310      -0.15599948
    ## 4  -0.1767662835  0.01844845  0.3613602805 -0.09371737      -0.14760381
    ## 5   0.3258363015  2.16613593  0.0454056765  1.69230235      -0.09456893
    ## 6   2.6208131254 -0.06367636  0.0776655646  0.18349938      -0.03715078
    ## 7  -0.1753145995  0.03062125 -0.0122984449  0.01710054      -0.24096389
    ## 8  -0.2717478133 -0.32252907 -0.1373392062 -0.31019111      -0.33106314
    ## 9  -0.2073392847 -0.09198145  0.2065182556 -0.15779222       2.12686463
    ## 10 -0.1007019535  0.18658414 -0.0095282441  0.09244824       0.12183236
    ##        fashion small_business        spam        adult
    ## 1  -0.09804505     0.22847023 -0.07768727 -0.022688821
    ## 2  -0.05906495     0.11380675 -0.07768727 -0.011970936
    ## 3  -0.01441204     0.83278058 -0.07768727 -0.047261963
    ## 4  -0.16605484     0.39919745 -0.07768727 -0.144741875
    ## 5   0.02913943     0.09397799 -0.07768727  0.003988664
    ## 6   2.67744986     0.16427476 -0.07768727 -0.019128740
    ## 7  -0.21613941    -0.16017031 -0.07768727 -0.109326046
    ## 8  -0.28981854    -0.21624197 -0.07768727 -0.011146248
    ## 9  -0.10504370    -0.11635574 -0.07768727  0.004939084
    ## 10 -0.02044987     0.31428826 12.41886450  3.750222155

``` r
clust1$center[1,]*sigma + mu
```

    ##   current_events           travel    photo_sharing          tv_film 
    ##     1.996795e+00     1.107906e+00     5.876068e+00     8.600427e-01 
    ##    sports_fandom         politics             food           family 
    ##     1.139957e+00     1.318376e+00     8.450855e-01     8.108974e-01 
    ##  home_and_garden            music             news    online_gaming 
    ##     6.335470e-01     8.194444e-01     6.196581e-01     7.489316e-01 
    ##         shopping health_nutrition      college_uni   sports_playing 
    ##     4.064103e+00     1.298077e+00     1.238248e+00     5.598291e-01 
    ##          cooking              eco        computers         business 
    ##     1.188034e+00     7.735043e-01     6.164530e-01     6.891026e-01 
    ##         outdoors           crafts       automotive              art 
    ##     4.316239e-01     5.822650e-01     9.166667e-01     3.685897e-01 
    ##         religion           beauty        parenting           dating 
    ##     5.982906e-01     4.209402e-01     6.143162e-01     9.583333e-01 
    ##           school personal_fitness          fashion   small_business 
    ##     8.130342e-01     9.081197e-01     8.173077e-01     4.775641e-01 
    ##             spam            adult 
    ##     9.540979e-18     3.621795e-01

``` r
clust1$center[2,]*sigma + mu
```

    ##   current_events           travel    photo_sharing          tv_film 
    ##     1.423398e+00     1.487465e+00     2.651811e+00     1.228412e+00 
    ##    sports_fandom         politics             food           family 
    ##     1.320334e+00     1.256267e+00     1.225627e+00     1.091922e+00 
    ##  home_and_garden            music             news    online_gaming 
    ##     5.710306e-01     6.323120e-01     7.994429e-01     1.080780e+01 
    ##         shopping health_nutrition      college_uni   sports_playing 
    ##     1.144847e+00     1.752089e+00     1.103343e+01     2.704735e+00 
    ##          cooking              eco        computers         business 
    ##     1.573816e+00     4.623955e-01     5.515320e-01     3.509749e-01 
    ##         outdoors           crafts       automotive              art 
    ##     6.100279e-01     5.348189e-01     9.080780e-01     1.161560e+00 
    ##         religion           beauty        parenting           dating 
    ##     7.325905e-01     4.011142e-01     7.103064e-01     7.103064e-01 
    ##           school personal_fitness          fashion   small_business 
    ##     5.097493e-01     1.008357e+00     8.885794e-01     4.066852e-01 
    ##             spam            adult 
    ##     5.204170e-17     3.816156e-01

``` r
clust1$center[4,]*sigma + mu
```

    ##   current_events           travel    photo_sharing          tv_film 
    ##     1.661972e+00     8.980282e+00     2.388732e+00     9.577465e-01 
    ##    sports_fandom         politics             food           family 
    ##     1.132394e+00     1.114930e+01     1.670423e+00     7.492958e-01 
    ##  home_and_garden            music             news    online_gaming 
    ##     5.549296e-01     6.338028e-01     3.571831e+00     7.549296e-01 
    ##         shopping health_nutrition      college_uni   sports_playing 
    ##     1.273239e+00     1.805634e+00     1.419718e+00     6.704225e-01 
    ##          cooking              eco        computers         business 
    ##     1.354930e+00     6.394366e-01     4.067606e+00     8.112676e-01 
    ##         outdoors           crafts       automotive              art 
    ##     7.408451e-01     6.816901e-01     6.478873e-01     4.591549e-01 
    ##         religion           beauty        parenting           dating 
    ##     1.309859e+00     4.704225e-01     9.492958e-01     1.354930e+00 
    ##           school personal_fitness          fashion   small_business 
    ##     6.563380e-01     1.107042e+00     6.929577e-01     5.830986e-01 
    ##             spam            adult 
    ##     5.117434e-17     1.408451e-01

To find which products are in which cluster, we run the following code

``` r
library(ggplot2)
theme_set(theme_bw())
Y = data.frame(raw.data)
Y$cluster = 0
for (i in 1:10){
  for (j in which(clust1$cluster == i)){
    Y[j,"cluster"] = i
  }
  
}
```

We constructed this ordered bar chart to see which categories are most
closest to which clusters. In some clusters, only 1 or 2 categories are
common, such as, in Cluster 6 where college uni and online gaming are
the only prevalent categories. However, in other clusters, there is a
good mix of equally important categories, such as, that in Cluster 1,
where categories like sports fanthom, religion, parenting and food are
common.

``` r
par(mfrow=c(5,2))
for (i in 1:10) {
  
  mask = Y$cluster==i
  temp = Y[,-length(Y)]
  value_count = rowSums(t(temp[mask,-1]))
  df = as.data.frame(value_count)
  df$index = rownames(df)
  #print(df)


# Draw plot
  print(ggplot(df ,aes(x=index, y=value_count)) + 
          geom_bar(stat="identity", width=.5, fill="tomato3") +
          labs(title="Bar Chart",
               subtitle="Categories Vs Value Counts") + 
          theme(axis.text.x = element_text(angle=65, vjust=0.6)))
}
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->

We also considered the CH index to look at possible estimates of K.
Since a good value for K in CH index is when the CH grid value is high,
we can conclude here that according to this method, k = 2 or k = 3 is a
reasonale k-value.

``` r
N = nrow(X)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=50)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}
```

    ## Warning: did not converge in 10 iterations
    
    ## Warning: did not converge in 10 iterations
    
    ## Warning: did not converge in 10 iterations

``` r
plot(k_grid, CH_grid)
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Using kmeans++, a random centroid value is chosen, and subsequent
cliusters are chosen on the basis of maximum distance between the
clusters. Here we can see a similar pattern as that in the ordered bar
chart above. Categories like online gaming and college uni are the only
ones important in one cluster, but categories like sports fandom and
religion have a number of other equally importamt categories in those
clusters.

``` r
# Using kmeans++ initialization
clust2 = kmeanspp(X, k=7, nstart=25)

Y = data.frame(raw.data)
Y$cluster = 0
for (i in 1:7){
  for (j in which(clust2$cluster == i)){
    Y[j,"cluster"] = i
  }
  
}
```

``` r
par(mfrow=c(5,2))
for (i in 1:7) {
  
  mask = Y$cluster==i
  temp = Y[,-length(Y)]
  value_count = rowSums(t(temp[mask,-1]))
  df = as.data.frame(value_count)
  df$index = rownames(df)
  #print(df)


# Draw plot
  print(ggplot(df ,aes(x=index, y=value_count)) + 
          geom_bar(stat="identity", width=.5, fill="tomato3") +
          labs(title="Bar Chart",
               subtitle="Categories Vs Value Counts") + 
          theme(axis.text.x = element_text(angle=65, vjust=0.6)))
}
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->![](Segmentation-Final_files/figure-gfm/unnamed-chunk-10-7.png)<!-- -->

We conduct hierarchical clustering to see if we can determine the
optimum number of clusters based on the proximity matrix. However,
hierarchical clustering does not work well her for a range of k-values

``` r
# Form a pairwise distance matrix using the dist function
distance_matrix = dist(X, method='euclidean')


# Now run hierarchical clustering
hier_X = hclust(distance_matrix, method='average')



# Plot the dendrogram
plot(hier_X, cex=0.8)
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Cut the tree into 5 clusters
clust3 = cutree(hier_X, k=37)
summary(factor(clust3))
```

    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 7656    4   11    3   69   46    2   16    4    8    2    2    1    6    1 
    ##   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30 
    ##    2    2    5    7    4    2    5    2    2    1    1    2    4    1    3 
    ##   31   32   33   34   35   36   37 
    ##    2    1    1    1    1    1    1

The average distance of the withinss of cluster 1 is around 18% of the
average distance of betweens of cluster 1. Hence, based on this data,
our clusters seem to be separated in an clear way.

``` r
# Compare versus within-cluster average distances for first two clusters
clust1$withinss
```

    ##  [1] 20608.472 10174.478 15245.829 11696.124 24125.889 16093.751 10561.062
    ##  [8] 40694.800 20369.809  2332.419

``` r
clust2$withinss
```

    ## [1] 74138.916 27698.931 24287.063 15699.808 19522.920 27346.394  2332.419

``` r
sum(clust1$withinss)
```

    ## [1] 171902.6

``` r
sum(clust2$withinss)
```

    ## [1] 191026.4

``` r
clust1$betweenss
```

    ## [1] 96051.37

``` r
clust2$betweenss
```

    ## [1] 76927.55

``` r
mean(clust1$withinss)
```

    ## [1] 17190.26

``` r
mean(clust2$withinss)
```

    ## [1] 27289.49

``` r
mean(clust1$betweenss)
```

    ## [1] 96051.37

``` r
mean(clust2$betweenss)
```

    ## [1] 76927.55

PCR We also tried PCR to look at the different kinds of users in our
market. We will look at the top 15 variables. In PC space, most of the
original observations end up near/close to zero. In the second graph, we
can see that categories such as sports fanthom, parenting and religion
are the most common one. Only one graph is shown here for context.

``` r
pc2 = prcomp(X, scale=TRUE, rank=2)
loadings = pc2$rotation
scores = pc2$x
```

``` r
qplot(scores[,1], scores[,2], color=Y$cluster, xlab='Component 1', ylab='Component 2')
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
barplot(loadings[,1], las = 2)
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The information in this data can be used to bring meaningful insights.
For example, if Nutrient H20 is planning to extend its product line or
diversify in new products, it can look for categories that are
consistent with its target market. For example, if Nutrient H20 plans to
target parents, I believe it can incorporate some aspect of food and
religion in its value chain so that its more reachable to parents.

Now we will look at correlation between categories which are not
distinct such as uncategorized and chatter with distinct
    categories.

``` r
cor.chatter <- cor(raw.data$chatter, raw.data[,-1])
```

``` r
cor(raw.data[,-1]$spam, raw.data[,-1])
```

    ##          chatter current_events     travel photo_sharing uncategorized
    ## [1,] 0.004603242     0.01940299 0.02277335  -0.008664858    0.01389805
    ##           tv_film sports_fandom    politics        food       family
    ## [1,] -0.004210646   0.008957464 0.009438922 0.001482766 -0.006802752
    ##      home_and_garden        music         news online_gaming    shopping
    ## [1,]      0.02160085 0.0005284301 -0.001800012   0.005867025 -0.01925162
    ##      health_nutrition college_uni sports_playing      cooking        eco
    ## [1,]      0.002395824 0.009452985   -0.008742556 -0.006178129 0.03141528
    ##       computers    business   outdoors     crafts  automotive        art
    ## [1,] 0.02311745 -0.02549185 0.02151895 0.01993976 0.008560904 0.02433927
    ##         religion       beauty  parenting      dating      school
    ## [1,] 0.008062977 -0.009135427 0.01609741 -0.00192651 0.006214842
    ##      personal_fitness      fashion small_business spam    adult
    ## [1,]       0.00787607 -0.003187322     0.03166214    1 0.294399

``` r
cor(raw.data[,-1]$uncategorized, raw.data[,-1])
```

    ##         chatter current_events     travel photo_sharing uncategorized
    ## [1,] 0.06699344     0.02967423 0.03086556     0.0963981             1
    ##       tv_film sports_fandom     politics       food       family
    ## [1,] 0.163279 -0.0005287314 -0.001143143 0.03534767 -0.004628184
    ##      home_and_garden     music        news online_gaming   shopping
    ## [1,]      0.07425914 0.1439077 0.003740334    0.02360168 0.05531255
    ##      health_nutrition college_uni sports_playing  cooking        eco
    ## [1,]       0.07985597  0.09471863     0.08383012 0.161257 0.04723203
    ##       computers   business   outdoors     crafts automotive       art
    ## [1,] 0.02663922 0.06596542 0.09390228 0.08919657 0.01391642 0.1064475
    ##        religion    beauty   parenting    dating     school
    ## [1,] 0.01703849 0.1373705 0.006268064 0.1270238 0.05825994
    ##      personal_fitness   fashion small_business       spam      adult
    ## [1,]       0.08472959 0.1414087     0.08483203 0.01389805 0.04519669

``` r
correlation <- round(cor(raw.data[,-1]),2)
```

``` r
library(reshape2)
melted_cormat <- melt(correlation, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Pearson\nCorrelation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 5, hjust = 1))+
 coord_fixed()
```

![](Segmentation-Final_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Photo-sharing and shopping are most related to chatter. As expected with
spam, the adult category is the most correlated. With uncategorized,
dating, cooking, tv-film and beauty are most correlated.

In the general correlation heatmap, the highest correlated categories
which have a correlation approximately equal to 1 are
(health\_nutrition, personal\_fitness) and (college\_uni, online
gaming). Since our company is a large consumer brand company,
correlation between categories such as photo-sharing and shopping can be
extremely useful to them. For example, closely related tweets in
categories such as photo sharing and shopping leads me to believe that
such customers would ideally follow bloggers who post pictures of
everything on social media. The company can use bloggers as one of their
marketing techniques.

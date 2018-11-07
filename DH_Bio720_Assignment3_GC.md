    #read in csv 
    rna_counts <- read.csv("https://www.dropbox.com/s/gqu520cc6r7xjw4/eXpress_dm_counts.csv?dl=1",  stringsAsFactors=FALSE)
    #filter out genes with expression of 0
    rna_counts <-  rna_counts[apply(rna_counts!=0, 1, all),]
    #create row names 
    rna_countsgood <- rna_counts[ , -1]
    rownames(rna_countsgood) <- rna_counts[,1]
    #create matrix - will be handy for some questions
    rna_countsgood <- data.matrix(rna_countsgood, rownames.force = NA)

1.  Create function that can calculate and output mean expression

<!-- -->

    #x argument is file and y argument is sample
    mean2 <- function(x, y, log2scale=TRUE) {
      if (log2scale == TRUE) {
        log2(mean(x[ , y ]))
      } else {
        mean(x[, y])
      }
    }

    #show that function works with log2 and without
    mean2(rna_counts, "F101_lg_female_hdhorn", log2scale = FALSE)

    ## [1] 2099.356

    mean2(rna_counts, "F101_lg_female_hdhorn", log2scale = TRUE)

    ## [1] 11.03573

    #show that function works with different sample 

    mean2(rna_counts, "M120_sm_male_genitalia", log2scale = TRUE)

    ## [1] 10.92715

1.  Use a loop to generate a vector of the mean expression value for
    each column

<!-- -->

    #change function to not require sample argument 

    mean3 <- function(x, log2scale=TRUE) {
      if (log2scale == TRUE) {
        log2(mean(x))
      } else {
        mean(x)
      }
    }

    #create loop to generate mean expression for each column 
    system.time(for (i in 2:56){
    z <- mean3(rna_counts[ , i]) 
    d <- colnames(rna_counts)[i]
    print (paste(d,":", z))
    }
    )

    ## [1] "F101_lg_female_hdhorn : 11.035730924265"
    ## [1] "F101_lg_female_thxhorn : 11.0362357920217"
    ## [1] "F101_lg_female_wings : 10.716752717884"
    ## [1] "F105_lg_female_hdhorn : 11.1192410821456"
    ## [1] "F105_lg_female_thxhorn : 10.5687453091957"
    ## [1] "F105_lg_female_wings : 10.9520804969188"
    ## [1] "F131_lg_female_hdhorn : 11.1342381496168"
    ## [1] "F131_lg_female_thxhorn : 11.2585260599595"
    ## [1] "F131_lg_female_wings : 11.2387791031338"
    ## [1] "F135_sm_female_wings : 10.8423560790346"
    ## [1] "F135_sm_female_hdhorn : 10.5831296762851"
    ## [1] "F135_sm_female_thxhorn : 10.8777810006906"
    ## [1] "F136_sm_female_hdhorn : 11.0980796429181"
    ## [1] "F136_sm_female_thxhorn : 10.883271328817"
    ## [1] "F136_sm_female_wings : 11.0460666472897"
    ## [1] "F196_sm_female_hdhorn : 10.4699306748831"
    ## [1] "F196_sm_female_thxhorn : 10.0884919842286"
    ## [1] "F196_sm_female_wings : 11.67113069508"
    ## [1] "F197_sm_female_hdhorn : 11.4367625734636"
    ## [1] "F197_sm_female_thxhorn : 11.0851986273641"
    ## [1] "F197_sm_female_wings : 11.1110442623127"
    ## [1] "F218_lg_female_hdhorn : 11.2726819972952"
    ## [1] "F218_lg_female_thxhorn : 11.0172586826345"
    ## [1] "F218_lg_female_wings : 11.1079566390334"
    ## [1] "M120_sm_male_genitalia : 10.9271536300526"
    ## [1] "M120_sm_male_hdhorn : 11.1288093746824"
    ## [1] "M120_sm_male_thxhorn : 11.1246081293535"
    ## [1] "M120_sm_male_wings : 11.3976117913299"
    ## [1] "M125_lg_male_genitalia : 11.1159924233683"
    ## [1] "M125_lg_male_hdhorn : 11.2985646155925"
    ## [1] "M125_lg_male_wings : 11.4096959821629"
    ## [1] "M160_lg_male_genitalia : 10.8422817053823"
    ## [1] "M160_lg_male_hdhorn : 11.1331989434806"
    ## [1] "M160_lg_male_thxhorn : 11.1165912357033"
    ## [1] "M160_lg_male_wings : 11.1818993051372"
    ## [1] "M171_sm_male_genitalia : 11.07610866972"
    ## [1] "M171_sm_male_hdhorn : 10.7254307381056"
    ## [1] "M171_sm_male_thxhorn : 10.7469660005696"
    ## [1] "M171_sm_male_wings : 10.9200719028322"
    ## [1] "M172_sm_male_genitalia : 11.18790077735"
    ## [1] "M172_sm_male_hdhorn : 10.8292879439499"
    ## [1] "M172_sm_male_thxhorn : 10.480294167073"
    ## [1] "M172_sm_male_wings : 11.4340429899223"
    ## [1] "M180_lg_male_genitalia : 10.9969993225733"
    ## [1] "M180_lg_male_hdhorn : 11.4683515240543"
    ## [1] "M180_lg_male_thxhorn : 11.0558504229551"
    ## [1] "M180_lg_male_wings : 11.7393157917179"
    ## [1] "M200_sm_male_genitalia : 11.323353138175"
    ## [1] "M200_sm_male_hdhorn : 11.0705274181503"
    ## [1] "M200_sm_male_thxhorn : 11.5495214369591"
    ## [1] "M200_sm_male_wings : 11.1938364793668"
    ## [1] "M257_lg_male_genitalia : 11.1716186263755"
    ## [1] "M257_lg_male_hdhorn : 11.2947592221444"
    ## [1] "M257_lg_male_thxhorn : 11.5140838547699"
    ## [1] "M257_lg_male_wings : 10.4613123915976"

    ##    user  system elapsed 
    ##   0.012   0.000   0.012

    #observe patterns 
    sort(log2(colMeans(rna_countsgood)))

    ## F196_sm_female_thxhorn     M257_lg_male_wings  F196_sm_female_hdhorn 
    ##               10.08849               10.46131               10.46993 
    ##   M172_sm_male_thxhorn F105_lg_female_thxhorn  F135_sm_female_hdhorn 
    ##               10.48029               10.56875               10.58313 
    ##   F101_lg_female_wings    M171_sm_male_hdhorn   M171_sm_male_thxhorn 
    ##               10.71675               10.72543               10.74697 
    ##    M172_sm_male_hdhorn M160_lg_male_genitalia   F135_sm_female_wings 
    ##               10.82929               10.84228               10.84236 
    ## F135_sm_female_thxhorn F136_sm_female_thxhorn     M171_sm_male_wings 
    ##               10.87778               10.88327               10.92007 
    ## M120_sm_male_genitalia   F105_lg_female_wings M180_lg_male_genitalia 
    ##               10.92715               10.95208               10.99700 
    ## F218_lg_female_thxhorn  F101_lg_female_hdhorn F101_lg_female_thxhorn 
    ##               11.01726               11.03573               11.03624 
    ##   F136_sm_female_wings   M180_lg_male_thxhorn    M200_sm_male_hdhorn 
    ##               11.04607               11.05585               11.07053 
    ## M171_sm_male_genitalia F197_sm_female_thxhorn  F136_sm_female_hdhorn 
    ##               11.07611               11.08520               11.09808 
    ##   F218_lg_female_wings   F197_sm_female_wings M125_lg_male_genitalia 
    ##               11.10796               11.11104               11.11599 
    ##   M160_lg_male_thxhorn  F105_lg_female_hdhorn   M120_sm_male_thxhorn 
    ##               11.11659               11.11924               11.12461 
    ##    M120_sm_male_hdhorn    M160_lg_male_hdhorn  F131_lg_female_hdhorn 
    ##               11.12881               11.13320               11.13424 
    ## M257_lg_male_genitalia     M160_lg_male_wings M172_sm_male_genitalia 
    ##               11.17162               11.18190               11.18790 
    ##     M200_sm_male_wings   F131_lg_female_wings F131_lg_female_thxhorn 
    ##               11.19384               11.23878               11.25853 
    ##  F218_lg_female_hdhorn    M257_lg_male_hdhorn    M125_lg_male_hdhorn 
    ##               11.27268               11.29476               11.29856 
    ## M200_sm_male_genitalia     M120_sm_male_wings     M125_lg_male_wings 
    ##               11.32335               11.39761               11.40970 
    ##     M172_sm_male_wings  F197_sm_female_hdhorn    M180_lg_male_hdhorn 
    ##               11.43404               11.43676               11.46835 
    ##   M257_lg_male_thxhorn   M200_sm_male_thxhorn   F196_sm_female_wings 
    ##               11.51408               11.54952               11.67113 
    ##     M180_lg_male_wings 
    ##               11.73932

male\_genitalia appears to have lower mean expression, compared to other
tissues in the same males. In addition, M180\_lg\_male\_wings has the
highest mean gene expression, while F196\_sm\_female\_thxhorn has the
lowest. I do not notice any major patterns for which individuals or
tissues have the highest mean expression.

4)Repeat above but using apply family function

    system.time(samplecounts <- sapply(rna_counts[, 2:56], mean3))

    ##    user  system elapsed 
    ##   0.001   0.000   0.001

    print(samplecounts)

    ##  F101_lg_female_hdhorn F101_lg_female_thxhorn   F101_lg_female_wings 
    ##               11.03573               11.03624               10.71675 
    ##  F105_lg_female_hdhorn F105_lg_female_thxhorn   F105_lg_female_wings 
    ##               11.11924               10.56875               10.95208 
    ##  F131_lg_female_hdhorn F131_lg_female_thxhorn   F131_lg_female_wings 
    ##               11.13424               11.25853               11.23878 
    ##   F135_sm_female_wings  F135_sm_female_hdhorn F135_sm_female_thxhorn 
    ##               10.84236               10.58313               10.87778 
    ##  F136_sm_female_hdhorn F136_sm_female_thxhorn   F136_sm_female_wings 
    ##               11.09808               10.88327               11.04607 
    ##  F196_sm_female_hdhorn F196_sm_female_thxhorn   F196_sm_female_wings 
    ##               10.46993               10.08849               11.67113 
    ##  F197_sm_female_hdhorn F197_sm_female_thxhorn   F197_sm_female_wings 
    ##               11.43676               11.08520               11.11104 
    ##  F218_lg_female_hdhorn F218_lg_female_thxhorn   F218_lg_female_wings 
    ##               11.27268               11.01726               11.10796 
    ## M120_sm_male_genitalia    M120_sm_male_hdhorn   M120_sm_male_thxhorn 
    ##               10.92715               11.12881               11.12461 
    ##     M120_sm_male_wings M125_lg_male_genitalia    M125_lg_male_hdhorn 
    ##               11.39761               11.11599               11.29856 
    ##     M125_lg_male_wings M160_lg_male_genitalia    M160_lg_male_hdhorn 
    ##               11.40970               10.84228               11.13320 
    ##   M160_lg_male_thxhorn     M160_lg_male_wings M171_sm_male_genitalia 
    ##               11.11659               11.18190               11.07611 
    ##    M171_sm_male_hdhorn   M171_sm_male_thxhorn     M171_sm_male_wings 
    ##               10.72543               10.74697               10.92007 
    ## M172_sm_male_genitalia    M172_sm_male_hdhorn   M172_sm_male_thxhorn 
    ##               11.18790               10.82929               10.48029 
    ##     M172_sm_male_wings M180_lg_male_genitalia    M180_lg_male_hdhorn 
    ##               11.43404               10.99700               11.46835 
    ##   M180_lg_male_thxhorn     M180_lg_male_wings M200_sm_male_genitalia 
    ##               11.05585               11.73932               11.32335 
    ##    M200_sm_male_hdhorn   M200_sm_male_thxhorn     M200_sm_male_wings 
    ##               11.07053               11.54952               11.19384 
    ## M257_lg_male_genitalia    M257_lg_male_hdhorn   M257_lg_male_thxhorn 
    ##               11.17162               11.29476               11.51408 
    ##     M257_lg_male_wings 
    ##               10.46131

A loop is slower than using the apply family function.

5)What is an easier way to do this?

    #use packages R has
    colMeans(rna_countsgood)

    ##  F101_lg_female_hdhorn F101_lg_female_thxhorn   F101_lg_female_wings 
    ##               2099.356               2100.091               1682.922 
    ##  F105_lg_female_hdhorn F105_lg_female_thxhorn   F105_lg_female_wings 
    ##               2224.463               1518.831               1981.092 
    ##  F131_lg_female_hdhorn F131_lg_female_thxhorn   F131_lg_female_wings 
    ##               2247.707               2449.932               2416.627 
    ##   F135_sm_female_wings  F135_sm_female_hdhorn F135_sm_female_thxhorn 
    ##               1836.007               1534.050               1881.648 
    ##  F136_sm_female_hdhorn F136_sm_female_thxhorn   F136_sm_female_wings 
    ##               2192.072               1888.822               2114.450 
    ##  F196_sm_female_hdhorn F196_sm_female_thxhorn   F196_sm_female_wings 
    ##               1418.284               1088.776               3261.072 
    ##  F197_sm_female_hdhorn F197_sm_female_thxhorn   F197_sm_female_wings 
    ##               2772.098               2172.588               2211.860 
    ##  F218_lg_female_hdhorn F218_lg_female_thxhorn   F218_lg_female_wings 
    ##               2474.090               2072.647               2207.131 
    ## M120_sm_male_genitalia    M120_sm_male_hdhorn   M120_sm_male_thxhorn 
    ##               1947.157               2239.265               2232.753 
    ##     M120_sm_male_wings M125_lg_male_genitalia    M125_lg_male_hdhorn 
    ##               2697.882               2219.459               2518.876 
    ##     M125_lg_male_wings M160_lg_male_genitalia    M160_lg_male_hdhorn 
    ##               2720.575               1835.913               2246.088 
    ##   M160_lg_male_thxhorn     M160_lg_male_wings M171_sm_male_genitalia 
    ##               2220.381               2323.203               2158.942 
    ##    M171_sm_male_hdhorn   M171_sm_male_thxhorn     M171_sm_male_wings 
    ##               1693.076               1718.538               1937.623 
    ## M172_sm_male_genitalia    M172_sm_male_hdhorn   M172_sm_male_thxhorn 
    ##               2332.887               1819.452               1428.509 
    ##     M172_sm_male_wings M180_lg_male_genitalia    M180_lg_male_hdhorn 
    ##               2766.877               2043.745               2833.465 
    ##   M180_lg_male_thxhorn     M180_lg_male_wings M200_sm_male_genitalia 
    ##               2128.838               3418.898               2562.530 
    ##    M200_sm_male_hdhorn   M200_sm_male_thxhorn     M200_sm_male_wings 
    ##               2150.606               2997.453               2342.505 
    ## M257_lg_male_genitalia    M257_lg_male_hdhorn   M257_lg_male_thxhorn 
    ##               2306.706               2512.241               2924.722 
    ##     M257_lg_male_wings 
    ##               1409.837

    log2(colMeans(rna_countsgood))

    ##  F101_lg_female_hdhorn F101_lg_female_thxhorn   F101_lg_female_wings 
    ##               11.03573               11.03624               10.71675 
    ##  F105_lg_female_hdhorn F105_lg_female_thxhorn   F105_lg_female_wings 
    ##               11.11924               10.56875               10.95208 
    ##  F131_lg_female_hdhorn F131_lg_female_thxhorn   F131_lg_female_wings 
    ##               11.13424               11.25853               11.23878 
    ##   F135_sm_female_wings  F135_sm_female_hdhorn F135_sm_female_thxhorn 
    ##               10.84236               10.58313               10.87778 
    ##  F136_sm_female_hdhorn F136_sm_female_thxhorn   F136_sm_female_wings 
    ##               11.09808               10.88327               11.04607 
    ##  F196_sm_female_hdhorn F196_sm_female_thxhorn   F196_sm_female_wings 
    ##               10.46993               10.08849               11.67113 
    ##  F197_sm_female_hdhorn F197_sm_female_thxhorn   F197_sm_female_wings 
    ##               11.43676               11.08520               11.11104 
    ##  F218_lg_female_hdhorn F218_lg_female_thxhorn   F218_lg_female_wings 
    ##               11.27268               11.01726               11.10796 
    ## M120_sm_male_genitalia    M120_sm_male_hdhorn   M120_sm_male_thxhorn 
    ##               10.92715               11.12881               11.12461 
    ##     M120_sm_male_wings M125_lg_male_genitalia    M125_lg_male_hdhorn 
    ##               11.39761               11.11599               11.29856 
    ##     M125_lg_male_wings M160_lg_male_genitalia    M160_lg_male_hdhorn 
    ##               11.40970               10.84228               11.13320 
    ##   M160_lg_male_thxhorn     M160_lg_male_wings M171_sm_male_genitalia 
    ##               11.11659               11.18190               11.07611 
    ##    M171_sm_male_hdhorn   M171_sm_male_thxhorn     M171_sm_male_wings 
    ##               10.72543               10.74697               10.92007 
    ## M172_sm_male_genitalia    M172_sm_male_hdhorn   M172_sm_male_thxhorn 
    ##               11.18790               10.82929               10.48029 
    ##     M172_sm_male_wings M180_lg_male_genitalia    M180_lg_male_hdhorn 
    ##               11.43404               10.99700               11.46835 
    ##   M180_lg_male_thxhorn     M180_lg_male_wings M200_sm_male_genitalia 
    ##               11.05585               11.73932               11.32335 
    ##    M200_sm_male_hdhorn   M200_sm_male_thxhorn     M200_sm_male_wings 
    ##               11.07053               11.54952               11.19384 
    ## M257_lg_male_genitalia    M257_lg_male_hdhorn   M257_lg_male_thxhorn 
    ##               11.17162               11.29476               11.51408 
    ##     M257_lg_male_wings 
    ##               10.46131

1.  Write function to get mean expression of given gene across samples

<!-- -->

    #create function
    mean4 <- function(x,y, log2scale=TRUE) {
      if (log2scale == TRUE) {
        log2(mean(x[ y, ]))
      } else {
        mean(x[ y,  ])
      }
    }

    #Show that function works
    mean4(rna_countsgood, "FBpp0087248")

    ## [1] 4.551796

    #Use apply to show each gene 
    genecounts <- apply(rna_countsgood, 1, mean3)
    print(genecounts)

    ## FBpp0087248 FBpp0293785 FBpp0080383 FBpp0077879 FBpp0311746 FBpp0289081 
    ##    4.551796   11.751088    6.313708    7.121204    7.180813   10.537130 
    ## FBpp0311729 FBpp0085807 FBpp0081078 FBpp0312037 FBpp0302581 FBpp0084962 
    ##   10.729466    7.520851    8.451286   10.424166   10.061138   10.660936 
    ## FBpp0311717 FBpp0301845 FBpp0070488 FBpp0070489 FBpp0070498 FBpp0079637 
    ##    5.751970    7.254127    8.668111    8.671139    9.008123   10.658601 
    ## FBpp0307731 FBpp0072041 FBpp0085933 FBpp0310022 FBpp0310023 FBpp0073761 
    ##    9.643922    9.081374    9.693107    7.762880    7.553333    8.744834 
    ## FBpp0075931 FBpp0293200 FBpp0076448 FBpp0076447 FBpp0290258 FBpp0087138 
    ##    6.915608    9.235797   11.408803   11.257216    9.649583    7.869131 
    ## FBpp0300338 FBpp0307649 FBpp0289682 FBpp0306972 FBpp0306971 FBpp0290399 
    ##    8.712371    8.589123    7.287083    9.470271    5.121015   11.046740 
    ## FBpp0301747 FBpp0070964 FBpp0307424 FBpp0071698 FBpp0070064 FBpp0309706 
    ##    9.919838   10.716134    9.619486   10.793840    9.429083   11.547902 
    ## FBpp0307826 FBpp0289160 FBpp0301573 FBpp0307274 FBpp0306212 FBpp0078893 
    ##    9.148500    9.032772   11.991290    8.864186   11.175379    7.753186 
    ## FBpp0297908 FBpp0084875 FBpp0072886 FBpp0304917 FBpp0084191 FBpp0300451 
    ##    9.330469    9.134893   11.799090   11.052951    8.701889    6.561271 
    ## FBpp0300454 FBpp0300453 FBpp0081890 FBpp0309414 FBpp0099609 FBpp0085609 
    ##    9.170790    9.505559   10.231068    9.478053    4.169925    7.642413 
    ## FBpp0292601 FBpp0304696 FBpp0087508 FBpp0087507 FBpp0306041 FBpp0088972 
    ##    8.373063    9.602851   11.512884   11.499121   10.810791   10.307097 
    ## FBpp0301166 FBpp0072256 FBpp0305245 FBpp0309235 FBpp0072048 FBpp0309965 
    ##   11.842800   10.096907    6.824814   12.989640    9.415281   10.212441 
    ## FBpp0072045 FBpp0290603 FBpp0079271 FBpp0292520 FBpp0110163 FBpp0076184 
    ##    8.082826   10.415223    8.309745    6.064130    9.887221    9.754645 
    ## FBpp0306282 FBpp0311985 FBpp0289242 FBpp0085890 FBpp0085891 FBpp0290686 
    ##    6.546474    8.541695    6.876405    6.697915   10.057844   12.183274 
    ## FBpp0290682 FBpp0311210 FBpp0110105 FBpp0305185 FBpp0086986 FBpp0079788 
    ##   12.128803   11.388056    5.631210    8.858094    7.532940   10.454299 
    ## FBpp0077502 FBpp0082980 FBpp0081609 FBpp0312210 FBpp0082235 FBpp0072429 
    ##   10.788867    8.910983    8.491999    8.966781    7.168649    6.719731 
    ## FBpp0072428 FBpp0305622 FBpp0072723 FBpp0080677 FBpp0309012 FBpp0086500 
    ##    8.983564    9.559673   10.704202    8.716804    8.958579    9.100519 
    ## FBpp0085074 FBpp0085571 FBpp0078694 FBpp0311413 FBpp0310585 FBpp0311962 
    ##    5.944007    9.417661    8.705852   10.692425    9.024485    8.879862 
    ## FBpp0291601 FBpp0111933 FBpp0306839 FBpp0077170 FBpp0304071 FBpp0300816 
    ##   10.125601    7.872381   10.214275   10.261518    9.211269   11.298927 
    ## FBpp0289422 FBpp0099946 FBpp0308662 FBpp0112011 FBpp0085743 FBpp0073204 
    ##    9.419271   10.288468    9.583937    9.047521    9.736709    8.365050 
    ## FBpp0290816 FBpp0305646 FBpp0290815 FBpp0111746 FBpp0086591 FBpp0073943 
    ##    7.550117    8.134333    8.943474   10.346393    9.236927   12.809633 
    ## FBpp0309126 FBpp0082056 FBpp0089363 FBpp0084619 FBpp0310244 FBpp0073059 
    ##    8.691077    7.969033   11.896725   10.199293    9.598628    9.283720 
    ## FBpp0293017 FBpp0072717 FBpp0300803 FBpp0070749 FBpp0297994 FBpp0099653 
    ##    9.674898   11.817870   13.050758   12.888563    8.110234    8.541977 
    ## FBpp0086707 FBpp0071379 FBpp0076580 FBpp0073194 FBpp0291143 FBpp0070024 
    ##    6.757314    9.906426    9.580824    7.774188   12.648194   11.796917 
    ## FBpp0305587 FBpp0072583 FBpp0304981 FBpp0083213 FBpp0088000 FBpp0087999 
    ##   11.287303   12.188673   10.837786    9.735879    6.677047    7.970812 
    ## FBpp0291368 FBpp0078971 FBpp0308407 FBpp0084733 FBpp0305264 FBpp0300417 
    ##   11.451324   10.090690    7.275109    9.082391   11.380284    5.631210 
    ## FBpp0297298 FBpp0081373 FBpp0082637 FBpp0079539 FBpp0078992 FBpp0311560 
    ##    9.986747    8.171563   10.978840    7.649354    8.856906   12.169857 
    ## FBpp0079682 FBpp0087045 FBpp0310441 FBpp0083105 FBpp0070453 FBpp0089160 
    ##    5.839776   11.217671    8.750692    9.537501   11.824742   11.654050 
    ## FBpp0309142 FBpp0303898 FBpp0290108 FBpp0306758 FBpp0083086 FBpp0304760 
    ##   11.270157   12.077004   11.198356    9.732676    6.314037    9.597511 
    ## FBpp0074808 FBpp0309380 FBpp0074995 FBpp0089115 FBpp0086267 FBpp0289662 
    ##    9.386450   12.258925    9.041410    9.586499    7.613505    8.327818 
    ## FBpp0081799 FBpp0306873 FBpp0306872 FBpp0292326 FBpp0307571 FBpp0088910 
    ##   11.219047    9.061727    9.123933   10.278280    8.954831    7.257902 
    ## FBpp0311477 FBpp0083936 FBpp0081352 FBpp0072250 FBpp0297127 FBpp0085358 
    ##   11.157485    7.107764    8.185326   10.641723   10.611427    8.191261 
    ## FBpp0288415 FBpp0306532 FBpp0099870 FBpp0290011 FBpp0300329 FBpp0293951 
    ##    6.607465   10.222597   10.753840   10.462354   10.338027    9.081955 
    ## FBpp0309983 FBpp0075086 FBpp0303197 FBpp0292502 FBpp0074330 FBpp0077346 
    ##    9.534391    9.890927    9.594087    8.664625    9.052469    9.879834 
    ## FBpp0309827 FBpp0079846 FBpp0081343 FBpp0071551 FBpp0077715 FBpp0082581 
    ##    8.027604    7.870925    8.994868    7.161338   10.971713   10.652894 
    ## FBpp0304099 FBpp0073673 FBpp0301197 FBpp0304675 FBpp0304032 FBpp0308470 
    ##   10.660028   10.944499   11.724041    8.985583    7.975822    5.674481 
    ## FBpp0306916 FBpp0306186 FBpp0306187 FBpp0310129 FBpp0073859 FBpp0087714 
    ##   10.004655    4.396060    4.113458    9.640475    9.742386    9.028258 
    ## FBpp0074784 FBpp0082972 FBpp0071129 FBpp0293470 FBpp0087686 FBpp0074247 
    ##    8.423135   10.802164   10.851124   10.133656   11.543067    8.917182 
    ## FBpp0309143 FBpp0073608 FBpp0307465 FBpp0309979 FBpp0289514 FBpp0081661 
    ##    9.962581    9.587863    9.854982   11.118634    6.470714    7.655092 
    ## FBpp0304298 FBpp0080997 FBpp0303820 FBpp0084021 FBpp0291142 FBpp0292665 
    ##    9.556576   10.160467    8.177012   10.194824    5.773229    9.020955 
    ## FBpp0292663 FBpp0075999 FBpp0112193 FBpp0309360 FBpp0305497 FBpp0292329 
    ##   10.805348    9.228469   11.758519    8.482790   11.660863    6.784932 
    ## FBpp0085812 FBpp0081156 FBpp0086395 FBpp0305484 FBpp0290814 FBpp0111781 
    ##    5.300124   11.937962   12.167965    7.983616   11.825833   11.037510 
    ## FBpp0084726 FBpp0087866 FBpp0290138 FBpp0290139 FBpp0307216 FBpp0079619 
    ##    9.990929    8.146418   11.111752   10.995819    7.114026    9.713308 
    ## FBpp0307931 FBpp0071548 FBpp0303265 FBpp0304880 FBpp0087429 FBpp0087431 
    ##    9.718922    9.870617    7.144380    6.496509   10.334253   10.359330 
    ## FBpp0289106 FBpp0087436 FBpp0074792 FBpp0308816 FBpp0297771 FBpp0308626 
    ##    9.994893   10.221587    9.154542    9.556436   10.584758    6.887304 
    ## FBpp0293582 FBpp0083533 FBpp0312441 FBpp0308362 FBpp0081263 FBpp0073740 
    ##    7.309580    8.516273   11.774929    8.340336    9.367713    9.073070 
    ## FBpp0308981 FBpp0309092 FBpp0074614 FBpp0071677 FBpp0292148 FBpp0302807 
    ##    8.280686   10.829795    7.509667    9.386019    9.451324   11.867721 
    ## FBpp0074314 FBpp0080450 FBpp0305020 FBpp0076185 FBpp0085075 FBpp0304800 
    ##    5.050739    7.910711    6.486597    7.540991   12.622380    6.138621 
    ## FBpp0311990 FBpp0301042 FBpp0085082 FBpp0081996 FBpp0304377 FBpp0081380 
    ##    8.272566    7.879862    8.445128   10.837356   13.570259    8.293367 
    ## FBpp0293890 FBpp0075918 FBpp0300990 FBpp0300989 FBpp0086347 FBpp0304174 
    ##    9.229780   13.388800   10.956931    9.558142    6.672682   10.862877 
    ## FBpp0088962 FBpp0072100 FBpp0309809 FBpp0110195 FBpp0100035 FBpp0305194 
    ##   11.609464    7.583322   10.010083    6.811798   10.596071    8.979048 
    ## FBpp0081850 FBpp0070795 FBpp0086647 FBpp0303580 FBpp0086868 FBpp0312526 
    ##    5.688282   10.265743    9.401190    7.463153    7.766258   10.368864 
    ## FBpp0291652 FBpp0074464 FBpp0071686 FBpp0307759 FBpp0291742 FBpp0081823 
    ##   13.547639    7.345990   10.135523   15.242374   11.073898   14.584714 
    ## FBpp0086483 FBpp0087352 FBpp0086506 FBpp0086505 FBpp0310686 FBpp0085586 
    ##    7.261838    7.191620    9.115871    9.055726   11.144438   13.854514 
    ## FBpp0308422 FBpp0072038 FBpp0071713 FBpp0292778 FBpp0309285 FBpp0309324 
    ##    9.468420    8.673903    7.049354    6.958843    6.861820    8.340255 
    ## FBpp0080672 FBpp0085448 FBpp0087704 FBpp0111946 FBpp0078881 FBpp0083584 
    ##   14.137381    6.973110   11.332810    8.672811    7.583596   11.468846 
    ## FBpp0070025 FBpp0086487 FBpp0085484 FBpp0309089 FBpp0312096 FBpp0306962 
    ##    9.895892   14.562535   13.545866   10.575917   10.419194    9.374170 
    ## FBpp0079737 FBpp0306292 FBpp0085804 FBpp0088841 FBpp0082876 FBpp0083537 
    ##    9.435954    9.812440    6.909729    7.719980   11.423441    9.670206 
    ## FBpp0292800 FBpp0305226 FBpp0089133 FBpp0290667 FBpp0297610 FBpp0304215 
    ##   10.231025    9.481909   12.173535    8.365766    9.418926    7.234578 
    ## FBpp0078566 FBpp0073437 FBpp0084340 FBpp0309079 FBpp0113120 FBpp0293289 
    ##   10.427218    9.327532   10.487365    9.326266   10.566590   12.388242 
    ## FBpp0290158 FBpp0310810 FBpp0073297 FBpp0085829 FBpp0088130 FBpp0080867 
    ##    7.691712    8.407384   10.782254    9.712121   10.115611    7.850158 
    ## FBpp0083979 FBpp0307180 FBpp0081526 FBpp0081990 FBpp0087180 FBpp0301778 
    ##   11.845768    7.755009    8.524346    8.787546    8.305937    5.715494 
    ## FBpp0070410 FBpp0072494 FBpp0303821 FBpp0073856 FBpp0079915 FBpp0305776 
    ##    8.923354   12.647597   11.090666   10.452990   12.145897   10.049131 
    ## FBpp0300799 FBpp0080484 FBpp0077367 FBpp0304732 FBpp0075316 FBpp0289785 
    ##    7.825161   11.344326   10.603137   10.854132   10.553437   12.498654 
    ## FBpp0289783 FBpp0078086 FBpp0303364 FBpp0303361 FBpp0303363 FBpp0075631 
    ##   11.404890    8.251633    7.809344   10.725165   10.730655    9.076913 
    ## FBpp0075425 FBpp0087320 FBpp0076872 FBpp0305817 FBpp0305335 FBpp0085453 
    ##    9.233576   11.224385   10.956521   11.668708   11.320565    7.340498 
    ## FBpp0077998 FBpp0292047 FBpp0074691 FBpp0306051 FBpp0087958 FBpp0303416 
    ##   10.510584    7.150486    9.970969    7.096883    9.336770   11.787057 
    ## FBpp0300431 FBpp0300432 FBpp0309386 FBpp0290266 FBpp0073664 FBpp0297358 
    ##   11.934066   11.901806   10.045884   10.106075    5.126282   12.131214 
    ## FBpp0084937 FBpp0072192 FBpp0271836 FBpp0087404 FBpp0293134 FBpp0079453 
    ##    5.936745   10.878358    9.563110    5.055480    6.630416   10.429673 
    ## FBpp0072172 FBpp0302579 FBpp0070083 FBpp0308713 FBpp0073678 FBpp0304879 
    ##    6.998154    7.747094    5.541695   10.566417    9.039667   15.339617 
    ## FBpp0305851 FBpp0290364 FBpp0305633 FBpp0081907 FBpp0081649 FBpp0310727 
    ##   10.663542    9.532019   10.969687   11.005880    6.999590   10.026850 
    ## FBpp0082180 FBpp0290906 FBpp0305244 FBpp0070465 FBpp0311868 FBpp0071296 
    ##    8.503392    6.988891    7.712496    7.516560    9.791341   10.898533 
    ## FBpp0305527 FBpp0289954 FBpp0084258 FBpp0308637 FBpp0077975 FBpp0084738 
    ##   11.202892   10.327267   10.252730    9.413589    7.518993   10.302826 
    ## FBpp0079326 FBpp0086359 FBpp0293627 FBpp0308762 FBpp0289849 FBpp0083767 
    ##   12.575311    6.729157    6.604772    8.625244    7.287922   10.090810 
    ## FBpp0077740 FBpp0290166 FBpp0298309 FBpp0082742 FBpp0080903 FBpp0311147 
    ##   11.523642    8.932081   11.006033    6.794416   12.616098   12.654823 
    ## FBpp0307273 FBpp0311874 FBpp0290830 FBpp0309990 FBpp0304417 FBpp0111524 
    ##    9.612265    7.647916    6.932242    8.036923   14.427915   10.474056 
    ## FBpp0075727 FBpp0305584 FBpp0075560 FBpp0292507 FBpp0084792 FBpp0084791 
    ##    8.314779    8.956310    8.818379   10.189757   10.841450    9.140807 
    ## FBpp0310240 FBpp0311780 FBpp0081226 FBpp0291807 FBpp0297588 FBpp0291806 
    ##   12.738219    6.701700   11.171507    9.919351    9.944007    9.289893 
    ## FBpp0080969 FBpp0087085 FBpp0302026 FBpp0289455 FBpp0079815 FBpp0083736 
    ##    9.248704    8.990078    9.058582    9.063836   10.767071    5.077398 
    ## FBpp0076871 FBpp0081961 FBpp0086475 FBpp0077872 FBpp0082186 FBpp0075617 
    ##    7.631342    7.972484    9.369062    9.510099   11.017973   11.390804 
    ## FBpp0292595 FBpp0297553 FBpp0074793 FBpp0080751 FBpp0080753 FBpp0308295 
    ##    9.353869   11.687614    5.532657   11.705640   11.641805    9.043301 
    ## FBpp0307682 FBpp0074442 FBpp0310438 FBpp0075214 FBpp0312219 FBpp0311549 
    ##    5.826895    9.209143    9.385784    4.245164    9.362701   11.134053 
    ## FBpp0085540 FBpp0089141 FBpp0110281 FBpp0303067 FBpp0303069 FBpp0084874 
    ##    9.475181   14.268804   12.563981    7.319959    7.310571    9.464936 
    ## FBpp0070612 FBpp0072127 FBpp0311256 FBpp0111705 FBpp0085693 FBpp0076791 
    ##    9.600521   12.017498   10.411915   10.966859    8.733662    9.249006 
    ## FBpp0289278 FBpp0292037 FBpp0271718 FBpp0312201 FBpp0074005 FBpp0087519 
    ##    8.958421    8.944060   10.620886    8.282541   10.684525    9.307967 
    ## FBpp0080275 FBpp0307803 FBpp0293626 FBpp0308661 FBpp0079623 FBpp0082720 
    ##    7.805011   11.472423    9.773259   11.257045    9.066040   11.081035 
    ## FBpp0297605 FBpp0086429 FBpp0081312 FBpp0311512 FBpp0307870 FBpp0088031 
    ##    6.843664   11.372667   11.659128    8.444676    9.115351    9.389270 
    ## FBpp0301215 FBpp0087323 FBpp0072564 FBpp0302692 FBpp0086421 FBpp0309704 
    ##   11.446933   10.353387   11.221126   11.215555   10.239685   10.597088 
    ## FBpp0310002 FBpp0302006 FBpp0304310 FBpp0076921 FBpp0290353 FBpp0078673 
    ##    8.440907    4.647000    7.518564    7.249135    8.999129    9.734094 
    ## FBpp0076599 FBpp0083846 FBpp0305307 FBpp0110549 FBpp0077448 FBpp0306201 
    ##    8.772150   11.474425    9.731196    8.985428    9.590655    8.538595 
    ## FBpp0306862 FBpp0080148 FBpp0289644 FBpp0292236 FBpp0310630 FBpp0292244 
    ##    8.271463    6.893053    9.603963   10.088186   10.154358   10.156979 
    ## FBpp0290516 FBpp0289110 FBpp0301793 FBpp0293619 FBpp0293617 FBpp0290577 
    ##   10.917589    3.944858    5.677047   10.618686   10.421128   10.939539 
    ## FBpp0082135 FBpp0071969 FBpp0071028 FBpp0079304 FBpp0291536 FBpp0289568 
    ##    8.159963    8.387312   10.309208    8.394580   11.318307    9.968510 
    ## FBpp0070977 FBpp0309146 FBpp0073154 FBpp0297431 FBpp0111550 FBpp0297369 
    ##    9.940994   10.462372    5.326511    6.867896    8.925352   11.336365 
    ## FBpp0292362 FBpp0076391 FBpp0076393 FBpp0076083 FBpp0085866 FBpp0309317 
    ##    8.184965   11.100435   11.976323    8.946189   12.293994   11.884101 
    ## FBpp0085865 FBpp0085869 FBpp0310296 FBpp0308526 FBpp0308430 FBpp0079802 
    ##   12.587236   12.252880    8.437052    9.011481    6.532940    9.693614 
    ## FBpp0301708 FBpp0291802 FBpp0081671 FBpp0311386 FBpp0304902 FBpp0310418 
    ##    6.848452    6.941448   11.434666   11.597130   11.506912   12.309466 
    ## FBpp0307951 FBpp0077750 FBpp0304134 FBpp0296949 FBpp0075348 FBpp0088490 
    ##    5.033223    8.196457    8.870197   11.801855    9.122569   11.601045 
    ## FBpp0088945 FBpp0071069 FBpp0303564 FBpp0072601 FBpp0304008 FBpp0307166 
    ##   11.651786    9.440794   13.655496    9.261582    9.135826    9.549662 
    ## FBpp0305415 FBpp0305414 FBpp0306018 FBpp0290840 FBpp0304449 FBpp0290720 
    ##    9.269722    9.360348    8.608876   10.230239    9.908638   11.701984 
    ## FBpp0303277 FBpp0085646 FBpp0303276 FBpp0076329 FBpp0081619 FBpp0086640 
    ##   10.802281   12.256825   11.686079    7.710869    8.813839    9.471638 
    ## FBpp0289951 FBpp0073067 FBpp0302570 FBpp0306848 FBpp0087585 FBpp0297612 
    ##    9.502741    8.335309   10.966492   11.000333   11.512723   12.496055 
    ## FBpp0113036 FBpp0079390 FBpp0074650 FBpp0086349 FBpp0293835 FBpp0081956 
    ##   13.443549    9.590451   11.345163   12.018390    6.766740    8.639785 
    ## FBpp0082107 FBpp0087010 FBpp0070874 FBpp0075088 FBpp0307982 FBpp0072975 
    ##    8.933798   10.637481    8.852941   10.388507   16.217937   10.023553 
    ## FBpp0087712 FBpp0074285 FBpp0291370 FBpp0288668 FBpp0087926 FBpp0083321 
    ##    8.330835    9.580344    7.064914   14.044920    6.737785    8.820063 
    ## FBpp0290811 FBpp0310381 FBpp0310380 FBpp0292261 FBpp0305198 FBpp0297444 
    ##   10.421969    9.649191    7.315685    5.554589    7.968196   10.546053 
    ## FBpp0290693 FBpp0297443 FBpp0085367 FBpp0100031 FBpp0312456 FBpp0084716 
    ##   10.319590   10.554502   11.003671   10.206215    8.362183    6.185506 
    ## FBpp0078142 FBpp0311638 FBpp0081810 FBpp0112110 FBpp0076076 FBpp0306019 
    ##    8.223914    7.636625    6.525556    9.162803    8.122334    9.444751 
    ## FBpp0305179 FBpp0292727 FBpp0292725 FBpp0289282 FBpp0072638 FBpp0082350 
    ##    8.382919    9.128580    9.091988   12.683122    9.294119   10.743304 
    ## FBpp0080823 FBpp0077964 FBpp0291724 FBpp0291723 FBpp0081322 FBpp0079524 
    ##    6.987445   10.254148   10.547490   11.363856    9.085146   12.105986 
    ## FBpp0073256 FBpp0081601 FBpp0075940 FBpp0079033 FBpp0111941 FBpp0080489 
    ##   14.185560   13.748281    7.265082    8.100041    9.389504    8.660093 
    ## FBpp0086316 FBpp0083855 FBpp0308734 FBpp0085064 FBpp0271781 FBpp0079253 
    ##    8.902457   11.492536    7.516273    9.989124   10.146210    6.410316 
    ## FBpp0079155 FBpp0300436 FBpp0307652 FBpp0074220 FBpp0074686 FBpp0074685 
    ##    5.280011    9.991779   11.158954    9.003735    8.859732    8.849533 
    ## FBpp0304337 FBpp0089048 FBpp0304336 FBpp0304339 FBpp0073801 FBpp0083988 
    ##    7.425197    7.122522    8.535982    7.909729    9.669239   10.030316 
    ## FBpp0077208 FBpp0289724 FBpp0290027 FBpp0083505 FBpp0076349 FBpp0072277 
    ##    8.896305    4.439019   10.181401   10.342600    9.817245   11.826057 
    ## FBpp0271909 FBpp0292671 FBpp0293411 FBpp0088182 FBpp0300576 FBpp0075344 
    ##    7.866099    9.447910    9.138621   10.546088    7.113079    9.991857 
    ## FBpp0311424 FBpp0086956 FBpp0078652 FBpp0099650 FBpp0084341 FBpp0086718 
    ##   11.341873   10.869566    8.170562    8.115257    8.460549   13.668466 
    ## FBpp0310303 FBpp0290493 FBpp0076346 FBpp0310079 FBpp0310199 FBpp0309007 
    ##   10.203882   10.910057    9.712496    7.865761   10.704705   11.135814 
    ## FBpp0290728 FBpp0074831 FBpp0311237 FBpp0307146 FBpp0311705 FBpp0081324 
    ##    9.710462   11.523811   10.321539    7.660677   12.010395   11.736279 
    ## FBpp0083453 FBpp0071077 FBpp0071078 FBpp0112020 FBpp0081813 FBpp0081404 
    ##    6.966414    9.946667    7.799488    8.228119    6.547596    9.459283 
    ## FBpp0291063 FBpp0291065 FBpp0310104 FBpp0291064 FBpp0071138 FBpp0081234 
    ##    4.712496    9.701480    9.845262    8.441963   10.609363    6.032822 
    ## FBpp0305104 FBpp0075122 FBpp0290613 FBpp0088350 FBpp0079247 FBpp0292965 
    ##   11.262469    8.411472   11.485774   10.949615   10.265317   11.028647 
    ## FBpp0298283 FBpp0081563 FBpp0308010 FBpp0072318 FBpp0311843 FBpp0308437 
    ##   12.591042    8.169925    8.691902    7.484109    8.462484    9.789059 
    ## FBpp0312545 FBpp0290714 FBpp0290713 FBpp0307414 FBpp0099512 FBpp0082228 
    ##    9.074531   10.849263   11.515807    7.263888    8.106622    7.814597 
    ## FBpp0271771 FBpp0292977 FBpp0306663 FBpp0304979 FBpp0088184 FBpp0308319 
    ##   10.166300   10.206659    8.898505   10.153920    9.062120   13.474261 
    ## FBpp0290729 FBpp0303602 FBpp0075170 FBpp0075171 FBpp0309151 FBpp0305606 
    ##    6.980192   12.735729   12.701570    9.617885   11.490258    9.148962 
    ## FBpp0311588 FBpp0312012 FBpp0082754 FBpp0077337 FBpp0301023 FBpp0080690 
    ##    8.179004    8.393722    5.671911    8.015288   10.684653    8.877410 
    ## FBpp0085457 FBpp0080719 FBpp0291768 FBpp0086484 FBpp0309269 FBpp0308322 
    ##    9.075795    9.103955   10.691141   11.335005   11.831264    8.191441 
    ## FBpp0078543 FBpp0070255 FBpp0088519 FBpp0079874 FBpp0309262 FBpp0089259 
    ##    6.845718    9.178914   10.227047   10.201010    9.150763    8.728168 
    ## FBpp0303140 FBpp0303512 FBpp0086531 FBpp0076342 FBpp0076341 FBpp0074558 
    ##    8.141596    9.994431    9.162849    8.642675    8.609750   11.923989 
    ## FBpp0112125 FBpp0304596 FBpp0082923 FBpp0310259 FBpp0080676 FBpp0305099 
    ##    7.993633    9.759132    8.956627   12.557085    9.844007    7.949004 
    ## FBpp0304216 FBpp0089221 FBpp0311249 FBpp0088498 FBpp0307026 FBpp0083163 
    ##    8.277985    9.885502    9.624978    7.588510    9.082730    7.613773 
    ## FBpp0307645 FBpp0079573 FBpp0300655 FBpp0084585 FBpp0289762 FBpp0289758 
    ##    7.823194    9.579179   14.212487   10.902471    9.731288    9.934173 
    ## FBpp0304847 FBpp0297624 FBpp0307553 FBpp0073651 FBpp0077713 FBpp0076438 
    ##   12.276907   11.449149   11.292876    8.499121    7.571787    9.658861 
    ## FBpp0309417 FBpp0071144 FBpp0111874 FBpp0075118 FBpp0310589 FBpp0077220 
    ##    9.874646   10.964210    6.678840    8.916205    7.221983   10.108406 
    ## FBpp0304993 FBpp0306197 FBpp0085216 FBpp0075916 FBpp0310202 FBpp0084955 
    ##   15.126328   10.007894    8.189196   11.815463    8.346796    4.739259 
    ## FBpp0081650 FBpp0076408 FBpp0072461 FBpp0084252 FBpp0074595 FBpp0304939 
    ##    9.011786   12.172763    9.008225   11.454262    8.931114    7.343569 
    ## FBpp0307855 FBpp0307451 FBpp0302864 FBpp0072874 FBpp0304148 FBpp0294043 
    ##    9.022923    9.588442    9.451586   11.271505    7.940380    6.205193 
    ## FBpp0309242 FBpp0079944 FBpp0075458 FBpp0099768 FBpp0071905 FBpp0305296 
    ##    7.028207    9.264528   10.008912    5.684717   10.368586   10.760508 
    ## FBpp0301223 FBpp0078935 FBpp0307930 FBpp0072629 FBpp0290774 FBpp0291126 
    ##   12.974480    6.986204    8.870981   11.262138   10.785542    8.856567 
    ## FBpp0083897 FBpp0310636 FBpp0309502 FBpp0290395 FBpp0290862 FBpp0078606 
    ##    9.988969   10.196859    7.684972    6.745873   10.988840   11.591208 
    ## FBpp0071254 FBpp0079663 FBpp0301123 FBpp0309427 FBpp0309943 FBpp0082187 
    ##    7.692600   13.891725   11.949462   11.477593    9.653855   10.675396 
    ## FBpp0088538 FBpp0312497 FBpp0076326 FBpp0291183 FBpp0077654 FBpp0310948 
    ##    9.763756    6.139737    6.252407    5.823657   10.000615   11.765120 
    ## FBpp0307565 FBpp0310187 FBpp0290522 FBpp0071973 FBpp0301840 FBpp0292193 
    ##    7.944752    9.340295    9.203682   10.071316   10.438489    8.791873 
    ## FBpp0304305 FBpp0310504 FBpp0312538 FBpp0308486 FBpp0073365 FBpp0307854 
    ##    8.118752    7.810280    6.904703   11.915859   11.319303    7.385745 
    ## FBpp0082065 FBpp0303087 FBpp0076824 FBpp0078363 FBpp0302626 FBpp0291679 
    ##   10.260385    7.552774    8.852657   10.068216   14.034751    9.765535 
    ## FBpp0071936 FBpp0307842 FBpp0077507 FBpp0086054 FBpp0088592 FBpp0086657 
    ##    8.382133   10.502018    9.679832    7.953562    4.138993    6.812498 
    ## FBpp0303934 FBpp0309866 FBpp0082719 FBpp0077133 FBpp0110463 FBpp0087645 
    ##   11.026108    7.486597    9.731720    8.703337   10.669272    8.724638 
    ## FBpp0292924 FBpp0302662 FBpp0311373 FBpp0075713 FBpp0304398 FBpp0290828 
    ##    9.781002    9.310406   12.093111   10.158931   10.114050   10.335370 
    ## FBpp0084174 FBpp0077868 FBpp0073672 FBpp0085713 FBpp0112042 FBpp0072863 
    ##    4.910384    8.693550    8.968510    8.771429   10.860480   10.848210 
    ## FBpp0303359 FBpp0088009 FBpp0307642 FBpp0297360 FBpp0081048 FBpp0070865 
    ##    8.170744    9.700377    9.844749    9.964577    8.728292   12.551547 
    ## FBpp0077362 FBpp0070584 FBpp0290710 FBpp0305007 FBpp0083611 FBpp0080704 
    ##   10.540762   10.911869   10.398005   12.543752   12.848939    9.526090 
    ## FBpp0309669 FBpp0111766 FBpp0084115 FBpp0071072 FBpp0308636 FBpp0297518 
    ##    8.880363    7.012853   11.007027   11.157393    8.702645    8.465529 
    ## FBpp0290035 FBpp0309825 FBpp0309714 FBpp0088493 FBpp0076879 FBpp0084580 
    ##   10.094318   10.721115   10.503501    7.345667    8.194578    7.603963 
    ## FBpp0070155 FBpp0089216 FBpp0087040 FBpp0089293 FBpp0079565 FBpp0071503 
    ##    8.171836   11.394092    8.873444    8.351301   12.358906    8.254986 
    ## FBpp0072557 FBpp0288575 FBpp0072590 FBpp0290366 FBpp0074971 FBpp0084739 
    ##    8.935138    9.906781    8.447309    9.712621    8.962055    7.563768 
    ## FBpp0305532 FBpp0082801 FBpp0070719 FBpp0292262 FBpp0305108 FBpp0084742 
    ##   12.572649    9.536689   10.324917    9.266446    8.006951    9.795066 
    ## FBpp0084350 FBpp0081153 FBpp0086469 FBpp0311431 FBpp0081355 FBpp0081756 
    ##    8.524916   18.520530    9.493746    9.852969   10.057746    9.571063 
    ## FBpp0074426 FBpp0088506 FBpp0081682 FBpp0087711 FBpp0309915 FBpp0070635 
    ##   10.035724   11.949707    9.787725    7.365527    5.381032   10.559082 
    ## FBpp0075519 FBpp0082832 FBpp0308474 FBpp0300972 FBpp0088877 FBpp0110197 
    ##    8.294119    9.338068    7.273414    7.838173   11.641042   11.549330 
    ## FBpp0073236 FBpp0307917 FBpp0081672 FBpp0309189 FBpp0071449 FBpp0297411 
    ##    8.729528   10.205193   13.079348    7.817391    8.469975    8.691077 
    ## FBpp0071752 FBpp0309391 FBpp0307248 FBpp0071194 FBpp0078334 FBpp0078333 
    ##    7.098415   12.544420    9.855548   10.206837    7.601399    9.078951 
    ## FBpp0309029 FBpp0086650 FBpp0071462 FBpp0309429 FBpp0306887 FBpp0081082 
    ##    9.955835   10.865030    7.901306    9.019944    8.751970    7.877856 
    ## FBpp0304769 FBpp0288449 FBpp0298003 FBpp0085308 FBpp0072782 FBpp0300747 
    ##    8.944486    7.817856    9.664690    9.237014    8.671911    6.090161 
    ## FBpp0306127 FBpp0309847 FBpp0084759 FBpp0310071 FBpp0307446 FBpp0290865 
    ##    8.508083   10.569165   10.948540    9.887774    7.935995   11.072108 
    ## FBpp0307388 FBpp0084748 FBpp0084750 FBpp0309852 FBpp0087918 FBpp0312411 
    ##   11.614484    9.211048   11.107312   10.984600    9.253740    9.613036 
    ## FBpp0088901 FBpp0079514 FBpp0291851 FBpp0304445 FBpp0077125 FBpp0084635 
    ##   10.050368    8.438111    7.178642    9.060843    7.867223    4.868795 
    ## FBpp0079666 FBpp0305602 FBpp0292906 FBpp0307720 FBpp0271936 FBpp0300820 
    ##    5.892833   10.286495    8.513046    8.919026   10.502651   11.200040 
    ## FBpp0085072 FBpp0084839 FBpp0087094 FBpp0292077 FBpp0308932 FBpp0086340 
    ##    9.723925    7.964210    9.245250    9.649517    8.771489   10.080139 
    ## FBpp0297608 FBpp0310748 FBpp0301183 FBpp0081613 FBpp0083244 FBpp0078472 
    ##    9.188656   10.722342   10.791281    3.885752    8.661195   10.585458 
    ## FBpp0078469 FBpp0306930 FBpp0297182 FBpp0310684 FBpp0312292 FBpp0309068 
    ##    9.395826    9.356632    9.194399    7.895589   10.915676    8.582228 
    ## FBpp0080048 FBpp0310139 FBpp0082545 FBpp0290896 FBpp0087084 FBpp0291294 
    ##   12.581243    9.310861    7.454804    4.107384    9.957313    9.315397 
    ## FBpp0070431 FBpp0310902 FBpp0289079 FBpp0074770 FBpp0076467 FBpp0087483 
    ##    9.192876    8.981386    9.149886    9.139086    9.404909    8.386215 
    ## FBpp0073659 FBpp0304173 FBpp0310396 FBpp0072956 FBpp0089217 FBpp0080191 
    ##   10.436560    9.555251    9.385235    7.970498    9.873053   12.674927 
    ## FBpp0293371 FBpp0078829 FBpp0078827 FBpp0311332 FBpp0311333 FBpp0297498 
    ##    7.239099   11.127373   13.143798    9.361864    8.985066    6.853224 
    ## FBpp0291374 FBpp0290947 FBpp0306506 FBpp0309978 FBpp0291853 FBpp0289912 
    ##    7.317824    8.082536   15.262949    5.532090    9.748101    8.538595 
    ## FBpp0077883 FBpp0308405 FBpp0073040 FBpp0309612 FBpp0311683 FBpp0310032 
    ##    5.188746   11.095457    6.233010   11.836194    8.777539    7.145677 
    ## FBpp0077029 FBpp0074113 FBpp0077738 FBpp0292775 FBpp0112025 FBpp0089139 
    ##    9.421109    8.458835    8.739689    6.669078   13.004454   13.025709 
    ## FBpp0297171 FBpp0290315 FBpp0076331 FBpp0300973 FBpp0310009 FBpp0082223 
    ##   11.009638   10.974532    9.393566    8.312718    9.017517   12.522229 
    ## FBpp0307187 FBpp0307186 FBpp0304407 FBpp0072899 FBpp0307805 FBpp0084244 
    ##    8.232399   11.208955    8.905524    8.913705    7.630813    9.457232 
    ## FBpp0112160 FBpp0308524 FBpp0072453 FBpp0073902 FBpp0300790 FBpp0304832 
    ##    8.338797    9.202925    9.276801   12.334050   11.013399    8.627235 
    ## FBpp0294034 FBpp0307426 FBpp0081896 FBpp0084486 FBpp0080495 FBpp0307142 
    ##    8.687328   11.523223   11.658309    7.518993   10.739167   10.704627 
    ## FBpp0080496 FBpp0079454 FBpp0075396 FBpp0077873 FBpp0075398 FBpp0087732 
    ##    9.132932   12.139534    7.355952    7.315685    9.904238    7.448661 
    ## FBpp0306672 FBpp0306632 FBpp0083179 FBpp0081055 FBpp0290679 FBpp0309338 
    ##   10.295581   10.717069   11.317433    9.424395    9.833523   10.070389 
    ## FBpp0087780 FBpp0111956 FBpp0308815 FBpp0304976 FBpp0111689 FBpp0305859 
    ##   10.881545    6.783266    9.946135   10.760493   10.385392    6.053901 
    ## FBpp0074836 FBpp0289451 FBpp0083935 FBpp0075248 FBpp0305488 FBpp0076488 
    ##    6.942088    8.793056    7.830011   11.708104   11.727990    9.909839 
    ## FBpp0111729 FBpp0076486 FBpp0297602 FBpp0076413 FBpp0292356 FBpp0301966 
    ##    9.884975    9.749138    9.875596    9.327491   10.712339    8.702708 
    ## FBpp0080392 FBpp0086590 FBpp0310569 FBpp0083225 FBpp0082624 FBpp0311839 
    ##    7.970394    9.458388    8.371717    6.401345    6.966623   10.110566 
    ## FBpp0078425 FBpp0307200 FBpp0082070 FBpp0084348 FBpp0110299 FBpp0074162 
    ##    7.745017    9.412743    7.804894    9.246287    6.648831   10.660158 
    ## FBpp0297167 FBpp0078402 FBpp0080017 FBpp0082352 FBpp0085616 FBpp0070373 
    ##    9.443810    7.685481    8.964735   10.918497    9.345748    9.235753 
    ## FBpp0309962 FBpp0297204 FBpp0311440 FBpp0289046 FBpp0070094 FBpp0306551 
    ##    9.345708   11.011761    7.034024    7.041609    7.065110   11.018491 
    ## FBpp0311451 FBpp0306904 FBpp0292890 FBpp0079182 FBpp0081434 FBpp0310065 
    ##   11.641658    9.640869    9.754827    8.208567    8.497816    9.863961 
    ## FBpp0290409 FBpp0305003 FBpp0290541 FBpp0110215 FBpp0110216 FBpp0289622 
    ##   11.453345    8.441511    6.953562    8.995741    8.979828    8.359630 
    ## FBpp0311786 FBpp0288892 FBpp0080237 FBpp0309425 FBpp0079519 FBpp0073717 
    ##   14.850676   11.020854   11.371014    9.507687    9.542610    9.576398 
    ## FBpp0080293 FBpp0082359 FBpp0074713 FBpp0294007 FBpp0075937 FBpp0302564 
    ##    8.521351    8.107003    6.280686    7.181356    9.878051   12.599131 
    ## FBpp0310151 FBpp0307613 FBpp0307614 FBpp0305795 FBpp0303428 FBpp0077343 
    ##    6.649093   10.618686   11.262490   11.426331   12.323147    9.391263 
    ## FBpp0293153 FBpp0308330 FBpp0074538 FBpp0074246 FBpp0289116 FBpp0078927 
    ##    9.158495    7.759858    9.146557    7.949217    8.442868   10.127901 
    ## FBpp0111580 FBpp0111581 FBpp0311993 FBpp0301283 FBpp0310514 FBpp0083814 
    ##    9.601703    8.268489    7.134893    8.901635    9.815674    5.498251 
    ## FBpp0309252 FBpp0309251 FBpp0085546 FBpp0070899 FBpp0308555 FBpp0073199 
    ##   10.256981    4.603424    9.246028   11.375869    4.964315   13.810064 
    ## FBpp0085617 FBpp0083879 FBpp0076533 FBpp0072033 FBpp0300997 FBpp0307448 
    ##    8.955993    8.691521    7.863961    9.376066    8.572063    7.763846 
    ## FBpp0305528 FBpp0309676 FBpp0289404 FBpp0082120 FBpp0075441 FBpp0081493 
    ##    8.092565    9.242611    8.688346    6.429616    9.413166    8.810455 
    ## FBpp0311646 FBpp0297092 FBpp0079847 FBpp0078372 FBpp0311009 FBpp0312439 
    ##    5.934173    7.770108    8.072437    9.314985   10.389680    7.366322 
    ## FBpp0085365 FBpp0082597 FBpp0291071 FBpp0084210 FBpp0073103 FBpp0076132 
    ##   11.530415   10.204905   12.265434    5.368387    9.399560    8.510027 
    ## FBpp0075935 FBpp0075934 FBpp0310637 FBpp0079527 FBpp0076784 FBpp0113091 
    ##   10.110875    9.536759   13.312978   12.342428    9.391927    9.404484 
    ## FBpp0290169 FBpp0081886 FBpp0087154 FBpp0300236 FBpp0083713 FBpp0084689 
    ##    7.512830   10.670737    9.251676   10.488936   10.318378   10.318707 
    ## FBpp0081473 FBpp0305823 FBpp0074206 FBpp0074205 FBpp0083413 FBpp0311406 
    ##   11.193828   11.170574    9.051085    8.967200   11.759018    9.426265 
    ## FBpp0111853 FBpp0071938 FBpp0071937 FBpp0085912 FBpp0311802 FBpp0089422 
    ##    7.193593    9.596732    9.829636    9.716554    9.752882   10.449036 
    ## FBpp0110316 FBpp0293227 FBpp0290866 FBpp0292613 FBpp0292910 FBpp0312193 
    ##   10.183026   11.402314    8.625377   10.533595   11.279990    7.367911 
    ## FBpp0076936 FBpp0305555 FBpp0301128 FBpp0301125 FBpp0300161 FBpp0084452 
    ##    7.472340    8.177555    8.729095    7.475291   11.410837   11.601518 
    ## FBpp0306270 FBpp0307572 FBpp0303191 FBpp0077893 FBpp0083885 FBpp0290185 
    ##   10.765580    7.271039   10.267872    8.707296    8.576605    9.403593 
    ## FBpp0080931 FBpp0078296 FBpp0311643 FBpp0076186 FBpp0297227 FBpp0311995 
    ##   12.114079    9.019439   12.800444    5.693867   11.116379   11.794541 
    ## FBpp0312093 FBpp0073588 FBpp0304873 FBpp0302777 FBpp0070330 FBpp0289493 
    ##   11.372390   10.207325    7.962002   12.121516   13.122122   11.052580 
    ## FBpp0290057 FBpp0311034 FBpp0071746 FBpp0305326 FBpp0311768 FBpp0311803 
    ##    7.267638   10.256123    8.445955   10.006594    9.648570    7.687392 
    ## FBpp0271832 FBpp0089163 FBpp0305662 FBpp0305522 FBpp0290099 FBpp0073128 
    ##    9.290438   10.164130    9.681015    7.800076    5.152331    7.600183 
    ## FBpp0070110 FBpp0290755 FBpp0078678 FBpp0078679 FBpp0078680 FBpp0290753 
    ##    6.430833   12.895892   13.516222   12.852324   12.848161   13.228767 
    ## FBpp0078674 FBpp0311450 FBpp0083227 FBpp0070652 FBpp0087744 FBpp0083842 
    ##   13.234828   14.106001   10.536954   10.538207   11.776897   10.631359 
    ## FBpp0310511 FBpp0074671 FBpp0297103 FBpp0307144 FBpp0082314 FBpp0080747 
    ##    7.302452   10.581269   10.893522   10.858363   10.623300    9.092709 
    ## FBpp0307178 FBpp0079254 FBpp0079255 FBpp0078797 FBpp0312226 FBpp0076718 
    ##    9.736648    6.633854    8.978996    4.567368   13.330726    8.764510 
    ## FBpp0311478 FBpp0070751 FBpp0301702 FBpp0071288 FBpp0084410 FBpp0290701 
    ##   10.060401    9.740578    9.765836    9.597003   10.060327   10.847200 
    ## FBpp0304333 FBpp0304672 FBpp0312207 FBpp0078116 FBpp0081608 FBpp0070730 
    ##    8.296374    8.328308    9.418428    8.225930    9.847342    7.813548 
    ## FBpp0079425 FBpp0072612 FBpp0083514 FBpp0083658 FBpp0300948 FBpp0309228 
    ##   11.341630    9.235840    7.041211    9.721876    9.595241   11.927738 
    ## FBpp0077899 FBpp0309275 FBpp0099795 FBpp0088033 FBpp0292920 FBpp0079347 
    ##    8.258073    8.249394    9.444902    7.803133    9.600014    9.205149 
    ## FBpp0309192 FBpp0307570 FBpp0306011 FBpp0080118 FBpp0310753 FBpp0081780 
    ##    8.010515    9.368229   10.170267   10.823541   11.059578   12.475411 
    ## FBpp0311199 FBpp0303185 FBpp0297586 FBpp0290579 FBpp0293283 FBpp0083436 
    ##    6.697410    7.325040   11.864439    6.212640    9.921652    9.592389 
    ## FBpp0083399 FBpp0075276 FBpp0072803 FBpp0293860 FBpp0310640 FBpp0305834 
    ##    8.687582   10.002866    9.373814    8.683505    6.341145    8.604098 
    ## FBpp0072562 FBpp0293533 FBpp0293530 FBpp0072906 FBpp0111528 FBpp0304182 
    ##   10.865129   10.034849    9.782671    9.987057   11.166802   10.543401 
    ## FBpp0074559 FBpp0309263 FBpp0302955 FBpp0304613 FBpp0077447 FBpp0085763 
    ##   10.497525    9.236493    9.903416   11.448670    9.544648    9.479266 
    ## FBpp0090954 FBpp0306434 FBpp0089292 FBpp0310440 FBpp0303715 FBpp0309362 
    ##    9.311810    8.892943    6.331731   10.250578    4.901635    9.921949 
    ## FBpp0305199 FBpp0076330 FBpp0288869 FBpp0311911 FBpp0307983 FBpp0302536 
    ##    7.504909    6.759737   10.041136   10.547122   12.839053    9.791133 
    ## FBpp0306001 FBpp0305775 FBpp0301158 FBpp0089033 FBpp0304835 FBpp0073620 
    ##    8.646935   11.941528    6.081278   13.053438   13.054937   10.563214 
    ## FBpp0089405 FBpp0088792 FBpp0292532 FBpp0307163 FBpp0080012 FBpp0312437 
    ##    7.334984   10.357412    8.713496    9.805685    9.569338   11.479863 
    ## FBpp0078642 FBpp0085657 FBpp0293054 FBpp0311547 FBpp0302962 FBpp0076520 
    ##   15.657258    7.684972    8.817623   11.386539    8.830818   10.699052 
    ## FBpp0073714 FBpp0070708 FBpp0070707 FBpp0310379 FBpp0086501 FBpp0074343 
    ##    9.576261   11.564470    6.975614    9.506317    9.941208   10.466030 
    ## FBpp0074342 FBpp0311662 FBpp0291423 FBpp0087398 FBpp0305083 FBpp0310796 
    ##   10.565725    4.156750    7.314202    9.272693   11.173700   12.352224 
    ## FBpp0080945 FBpp0072475 FBpp0070798 FBpp0085809 FBpp0084176 FBpp0305448 
    ##   10.865452    9.087463   13.835471   11.064975   11.943760   10.375829 
    ## FBpp0306905 FBpp0112097 FBpp0311408 FBpp0297108 FBpp0311758 FBpp0305422 
    ##   10.687853    6.413089    7.098224    6.761430    8.785765    6.780644 
    ## FBpp0075680 FBpp0087544 FBpp0290074 FBpp0087535 FBpp0081130 FBpp0289747 
    ##   10.810776    9.777001   10.668402    9.755464    8.104051   10.870771 
    ## FBpp0309398 FBpp0309396 FBpp0288679 FBpp0302956 FBpp0271928 FBpp0073823 
    ##    9.790719    8.155278    9.131483    9.271887    7.105099   12.211424 
    ## FBpp0079924 FBpp0087855 FBpp0077925 FBpp0080817 FBpp0311225 FBpp0071217 
    ##   11.148754    7.685609   11.005982    9.475254   10.065355   10.240878 
    ## FBpp0070006 FBpp0070355 FBpp0083613 FBpp0306504 FBpp0077119 FBpp0311657 
    ##   10.433263   11.945071    6.938671    9.274431    9.473521   12.143418 
    ## FBpp0309217 FBpp0075794 FBpp0074401 FBpp0293060 FBpp0297302 FBpp0289196 
    ##    8.540991   11.942654   11.763522   10.474738   11.829716   14.746446 
    ## FBpp0307245 FBpp0289199 FBpp0290285 FBpp0070041 FBpp0082985 FBpp0293233 
    ##   13.937787   14.741488   11.363995   11.597121   11.585039    9.519994 
    ## FBpp0070811 FBpp0077867 FBpp0301152 FBpp0307374 FBpp0088044 FBpp0310230 
    ##    9.506353    8.842065   13.788405   10.503952    8.984496   10.869384 
    ## FBpp0297726 FBpp0312446 FBpp0074915 FBpp0074917 FBpp0086664 FBpp0308325 
    ##    8.834385   12.556044   12.589145   12.565444    8.341711    9.348125 
    ## FBpp0099896 FBpp0291730 FBpp0291729 FBpp0111759 FBpp0311113 FBpp0072567 
    ##   10.513405   14.114846    8.458984    8.162986    8.823251   11.660701 
    ## FBpp0072568 FBpp0083686 FBpp0290295 FBpp0309318 FBpp0088429 FBpp0291866 
    ##   11.578364    9.102238    9.932537   11.639357    9.258116    9.115729 
    ## FBpp0290196 FBpp0290195 FBpp0073247 FBpp0073246 FBpp0079042 FBpp0303230 
    ##    9.474075    5.842522    7.604098    8.297291    4.191620    8.431137 
    ## FBpp0082592 FBpp0308326 FBpp0298033 FBpp0293201 FBpp0303033 FBpp0071049 
    ##    9.858716   10.837513    9.588169   12.085973    8.584416    8.967881 
    ## FBpp0112712 FBpp0112713 FBpp0304252 FBpp0076892 FBpp0073893 FBpp0304930 
    ##   10.044171    9.430072   11.480221   11.141921    9.927239    8.509955 
    ## FBpp0304921 FBpp0304920 FBpp0304314 FBpp0310097 FBpp0075360 FBpp0080045 
    ##   11.196133   11.835605    9.017011    8.610555    9.688727    9.018478 
    ## FBpp0082055 FBpp0080915 FBpp0086116 FBpp0310439 FBpp0303068 FBpp0297935 
    ##    7.225492    8.711370   10.207258    7.131904    9.056564   11.044059 
    ## FBpp0307449 FBpp0082935 FBpp0112331 FBpp0302987 FBpp0310714 FBpp0083082 
    ##    7.968091    7.814014   11.531443    8.627700    9.413051   14.235257 
    ## FBpp0076385 FBpp0271876 FBpp0312324 FBpp0292158 FBpp0302934 FBpp0307685 
    ##   15.710392    8.616916    8.921516   10.879611   10.569769    8.173018 
    ## FBpp0300173 FBpp0088654 FBpp0077520 FBpp0075038 FBpp0077503 FBpp0305130 
    ##    8.355711    8.251891    8.021863    9.013564   10.756693   10.607448 
    ## FBpp0298330 FBpp0298328 FBpp0082036 FBpp0082034 FBpp0303610 FBpp0082035 
    ##    8.743366    7.646084    7.830933    5.442642    7.857981    6.410933 
    ## FBpp0297342 FBpp0297339 FBpp0087069 FBpp0293018 FBpp0311368 FBpp0083958 
    ##   10.870183   10.951510    6.701448    8.106147    7.034824    8.092853 
    ## FBpp0300568 FBpp0071192 FBpp0087058 FBpp0309260 FBpp0072148 FBpp0072952 
    ##    9.778615   11.931980   11.540648    9.090305    7.932027    9.911310 
    ## FBpp0079770 FBpp0305544 FBpp0087353 FBpp0305265 FBpp0293000 FBpp0307992 
    ##    9.069267    7.204838   10.266552   12.223519   10.505559   11.107788 
    ## FBpp0292404 FBpp0293465 FBpp0307387 FBpp0087120 FBpp0297105 FBpp0297107 
    ##   10.950887   10.499121   11.527877    8.103288   10.793441    7.168285 
    ## FBpp0304785 FBpp0080319 FBpp0304830 FBpp0085870 FBpp0302018 FBpp0312568 
    ##   10.077374    9.444450   12.261897   10.904101   10.527548   10.234861 
    ## FBpp0073848 FBpp0305206 FBpp0310741 FBpp0292340 FBpp0078920 FBpp0307630 
    ##    8.539370   10.395904   11.949137    7.340336    8.579350   11.256123 
    ## FBpp0310809 FBpp0292773 FBpp0307564 FBpp0305270 FBpp0072006 FBpp0079156 
    ##    8.808174    8.265594    8.294871   11.393078    6.972275    9.635768 
    ## FBpp0080862 FBpp0070216 FBpp0309594 FBpp0076654 FBpp0304343 FBpp0308578 
    ##    8.084180   10.440586    8.778436   10.130688   13.360765    7.237884 
    ## FBpp0293051 FBpp0078722 FBpp0078723 FBpp0290848 FBpp0290847 FBpp0081509 
    ##   15.191069    8.399016    8.229605    9.949402    7.717988   11.136187 
    ## FBpp0111835 FBpp0078268 FBpp0073996 FBpp0311495 FBpp0073674 FBpp0087406 
    ##   10.291234   15.464868   10.647851   10.608053   10.269105    8.300706 
    ## FBpp0305988 FBpp0071406 FBpp0312156 FBpp0291523 FBpp0300977 FBpp0084089 
    ##    8.566884    8.380874    8.654115    8.097936    6.764329   11.160479 
    ## FBpp0085117 FBpp0111932 FBpp0077316 FBpp0288548 FBpp0288731 FBpp0078648 
    ##    8.126564    8.779033    9.363498    9.939926    9.263291    7.619253 
    ## FBpp0297777 FBpp0303837 FBpp0076549 FBpp0099631 FBpp0306416 FBpp0099632 
    ##    7.553193    7.372351    9.380638    8.678904    9.590009    9.518707 
    ## FBpp0301556 FBpp0075675 FBpp0081447 FBpp0300995 FBpp0078182 FBpp0288833 
    ##    9.553856    6.803838    7.924705    6.044394    9.549907   10.442114 
    ## FBpp0080280 FBpp0077262 FBpp0290850 FBpp0300975 FBpp0308852 FBpp0306093 
    ##   14.081974   11.455579    5.163352    6.199494   11.214595   10.262501 
    ## FBpp0086732 FBpp0300976 FBpp0110523 FBpp0110483 FBpp0073449 FBpp0071298 
    ##    6.185146    9.811097    9.632599    6.681143   11.962344    8.560715 
    ## FBpp0075693 FBpp0305742 FBpp0075239 FBpp0085765 FBpp0308364 FBpp0071279 
    ##    9.942941    7.137317    9.689712    7.357072    7.858998   12.313651 
    ## FBpp0301591 FBpp0088357 FBpp0309307 FBpp0074026 FBpp0088013 FBpp0290003 
    ##    7.044394    8.411010    8.975770   10.205727    9.103145    9.188926 
    ## FBpp0297426 FBpp0071700 FBpp0070224 FBpp0074788 FBpp0311541 FBpp0082664 
    ##    9.813227    7.969871    9.067117   10.386254   10.578493   10.361485 
    ## FBpp0302792 FBpp0305673 FBpp0071115 FBpp0078166 FBpp0309959 FBpp0070831 
    ##    9.451548    9.969923   11.977098   11.889200    9.927185   11.022280 
    ## FBpp0071418 FBpp0300807 FBpp0075115 FBpp0079819 FBpp0082550 FBpp0297136 
    ##   11.137504   11.140656    9.485902   10.297813   10.171927    3.862496 
    ## FBpp0305313 FBpp0086941 FBpp0311473 FBpp0079685 FBpp0071212 FBpp0297401 
    ##   12.260481   11.237731   10.469346    9.563352   13.431146   11.547114 
    ## FBpp0309997 FBpp0085715 FBpp0293217 FBpp0074936 FBpp0084907 FBpp0088054 
    ##   10.553525    8.704470    9.112984    8.925244   12.370460    7.695007 
    ## FBpp0083932 FBpp0070876 FBpp0077302 FBpp0309145 FBpp0078358 FBpp0088561 
    ##    9.566400    7.619786    8.363856    8.187577    8.618586   11.265381 
    ## FBpp0293844 FBpp0293837 FBpp0308484 FBpp0309944 FBpp0077022 FBpp0077099 
    ##   10.355471    9.451586    8.164633    9.605614    7.570822    7.702204 
    ## FBpp0088818 FBpp0084213 FBpp0311052 FBpp0291660 FBpp0309024 FBpp0289975 
    ##    9.221280    7.150486   10.549190    9.782462    9.812877   10.465641 
    ## FBpp0082989 FBpp0080125 FBpp0080124 FBpp0084043 FBpp0075096 FBpp0074104 
    ##   13.781907   10.023780   10.108263    7.130968   10.018934   10.107907 
    ## FBpp0082541 FBpp0079549 FBpp0072239 FBpp0290893 FBpp0304044 FBpp0289202 
    ##   11.325939    9.840950   10.016631   15.033208   12.703097    8.108334 
    ## FBpp0307782 FBpp0305102 FBpp0085589 FBpp0310242 FBpp0071849 FBpp0071269 
    ##    7.450161   10.864721    8.361944   10.596715    8.735387    9.800106 
    ## FBpp0078843 FBpp0307972 FBpp0082855 FBpp0076485 FBpp0300612 FBpp0074568 
    ##    9.291652    9.091988    7.536618    9.704721   15.768521   10.827227 
    ## FBpp0309288 FBpp0082148 FBpp0303793 FBpp0086939 FBpp0082813 FBpp0084852 
    ##   10.032697    8.954143   11.743718    9.881726    9.780793   15.561344 
    ## FBpp0084729 FBpp0075186 FBpp0306920 FBpp0303568 FBpp0303566 FBpp0303567 
    ##    8.786834    9.087366    9.886639   10.641510   11.035099   10.766845 
    ## FBpp0303572 FBpp0079844 FBpp0304186 FBpp0079699 FBpp0311506 FBpp0303939 
    ##    8.151501    5.658990    8.258416    9.415319    7.317166    9.467494 
    ## FBpp0297063 FBpp0099825 FBpp0309030 FBpp0289825 FBpp0310679 FBpp0290777 
    ##   10.740624   10.403477   12.494337    8.500788    7.506353    8.195831 
    ## FBpp0306931 FBpp0306932 FBpp0111703 FBpp0079621 FBpp0079620 FBpp0099403 
    ##   11.840599   11.951834   13.513952   11.073740   10.982981    8.552425 
    ## FBpp0305725 FBpp0294008 FBpp0073262 FBpp0312024 FBpp0305599 FBpp0308632 
    ##    8.753916    9.660547   10.055973   10.569113   10.521047    7.256016 
    ## FBpp0310095 FBpp0081255 FBpp0311801 FBpp0088552 FBpp0088549 FBpp0088550 
    ##    9.774488    8.275278    8.338716    8.541554    6.438716    8.559603 
    ## FBpp0088551 FBpp0311398 FBpp0297162 FBpp0289985 FBpp0289573 FBpp0074230 
    ##    5.530956   13.099485   10.815849    8.043698    7.742815   10.757875 
    ## FBpp0074365 FBpp0311684 FBpp0074145 FBpp0074144 FBpp0078189 FBpp0309136 
    ##   10.998449    7.754402    7.556123    7.489374    7.622851   11.589711 
    ## FBpp0312067 FBpp0311350 FBpp0071676 FBpp0077724 FBpp0082198 FBpp0304357 
    ##    8.403438   14.050053   10.184222    9.430224   11.914372   10.424128 
    ## FBpp0076244 FBpp0290592 FBpp0302852 FBpp0084974 FBpp0307415 FBpp0082030 
    ##   11.247906    5.911692   12.295832    9.861172   10.027529    4.087463 
    ## FBpp0300571 FBpp0289826 FBpp0082642 FBpp0305826 FBpp0086416 FBpp0099994 
    ##    9.366561    8.930684    9.268659    6.676021   10.159023    8.993016 
    ## FBpp0303192 FBpp0078161 FBpp0304320 FBpp0083033 FBpp0073935 FBpp0304105 
    ##    7.992192    9.723087    6.107384   10.895327    6.885752    8.415089 
    ## FBpp0309250 FBpp0310006 FBpp0071624 FBpp0301213 FBpp0084688 FBpp0291670 
    ##    8.105766   10.966243   10.916734   10.758677    9.587283    9.141457 
    ## FBpp0289873 FBpp0080559 FBpp0080560 FBpp0082061 FBpp0300539 FBpp0099784 
    ##    8.289684    8.677495    9.157668    7.027001    9.505523    9.641264 
    ## FBpp0070202 FBpp0073530 FBpp0075120 FBpp0305621 FBpp0113023 FBpp0088471 
    ##    8.977124    7.972066    7.783266    7.853224    6.257902   10.457885 
    ## FBpp0304171 FBpp0089164 FBpp0309972 FBpp0297436 FBpp0075677 FBpp0305943 
    ##   12.300363   13.119802    9.377211    8.603424   10.620936    9.719638 
    ## FBpp0077012 FBpp0305095 FBpp0072781 FBpp0081600 FBpp0081310 FBpp0311267 
    ##   11.772255   14.816005   10.282141    9.245164    9.016859   10.276780 
    ## FBpp0073976 FBpp0112329 FBpp0074702 FBpp0310405 FBpp0308016 FBpp0088478 
    ##    9.476028   12.100961   14.939788    3.934602   13.154574    7.166642 
    ## FBpp0304476 FBpp0292427 FBpp0301586 FBpp0071277 FBpp0078450 FBpp0308997 
    ##   11.125766   13.710356    9.180180    9.868710    9.138155    7.241355 
    ## FBpp0083007 FBpp0078404 FBpp0089179 FBpp0305722 FBpp0297580 FBpp0086016 
    ##    6.964105    9.183523   10.065453    7.132839   10.618386    8.222773 
    ## FBpp0309558 FBpp0089192 FBpp0085524 FBpp0308657 FBpp0308658 FBpp0085694 
    ##    7.955148   10.205238   10.870084    8.297542    8.063346   13.252198 
    ## FBpp0087756 FBpp0072177 FBpp0085720 FBpp0309138 FBpp0309137 FBpp0312008 
    ##    8.133119   15.117438   18.007988    9.450649   10.721705   10.935259 
    ## FBpp0077073 FBpp0077074 FBpp0305363 FBpp0293539 FBpp0077625 FBpp0074847 
    ##    7.266105    7.299624   10.086764    9.251289    8.918538    7.560993 
    ## FBpp0075498 FBpp0311978 FBpp0099868 FBpp0077896 FBpp0290552 FBpp0304691 
    ##   11.880272   13.300652    8.411472    9.000768   10.894074   11.221115 
    ## FBpp0292227 FBpp0309795 FBpp0073135 FBpp0110561 FBpp0082798 FBpp0076375 
    ##   11.176898    8.706543    8.840349    8.944326    8.862215   10.284898 
    ## FBpp0303522 FBpp0079757 FBpp0084926 FBpp0084678 FBpp0291398 FBpp0302943 
    ##    8.425044   10.762427    6.417392   11.839826   12.084011    7.694880 
    ## FBpp0291373 FBpp0099801 FBpp0302948 FBpp0301784 FBpp0075952 FBpp0075785 
    ##    8.056465    8.604301    8.987548    8.519779    9.896195   10.772839 
    ## FBpp0075071 FBpp0087646 FBpp0081840 FBpp0072977 FBpp0309742 FBpp0310070 
    ##    7.315685    9.364413    9.000871    9.377960    7.203415   14.549820 
    ## FBpp0293997 FBpp0086427 FBpp0111676 FBpp0071536 FBpp0290444 FBpp0074453 
    ##    9.200386    9.212905    9.683665    7.822614    9.418581    7.913433 
    ## FBpp0081280 FBpp0307212 FBpp0076461 FBpp0073874 FBpp0310509 FBpp0307038 
    ##    9.591471   10.118870    8.615446    8.397227    9.112842    9.179547 
    ## FBpp0086503 FBpp0086669 FBpp0300754 FBpp0288750 FBpp0309393 FBpp0309394 
    ##   10.877465   11.312460   11.233609   10.433984    9.134893   10.497435 
    ## FBpp0292044 FBpp0311694 FBpp0300207 FBpp0306617 FBpp0081412 FBpp0311120 
    ##    7.212109   10.080454    6.624845   11.107871   10.158472    5.818553 
    ## FBpp0089099 FBpp0306427 FBpp0297861 FBpp0312556 FBpp0072175 FBpp0072176 
    ##    5.813898   10.294913    9.587249    6.752457    9.774368    9.953456 
    ## FBpp0309841 FBpp0081010 FBpp0305300 FBpp0081031 FBpp0297159 FBpp0311540 
    ##   11.262661   10.902224   10.421128   11.025945   10.560229    8.122804 
    ## FBpp0074076 FBpp0077206 FBpp0309042 FBpp0302561 FBpp0077193 FBpp0084662 
    ##    9.913406   11.538974    8.838631   12.945623    8.304694   10.884490 
    ## FBpp0071256 FBpp0306949 FBpp0087514 FBpp0081004 FBpp0305755 FBpp0075275 
    ##    9.056859    8.908529   11.110175    6.052321    8.959633   10.493892 
    ## FBpp0082569 FBpp0079305 FBpp0305784 FBpp0075267 FBpp0112507 FBpp0073359 
    ##    9.748101   12.163655    9.623749    9.069120   10.736786    7.769026 
    ## FBpp0306285 FBpp0309690 FBpp0076990 FBpp0310005 FBpp0077624 FBpp0288476 
    ##    6.909293    9.928263    9.682612    5.833810    7.597071   10.547613 
    ## FBpp0086114 FBpp0297907 FBpp0305594 FBpp0311378 FBpp0070677 FBpp0301227 
    ##    9.618085   12.835491   10.273647    7.864074   10.393878   10.349110 
    ## FBpp0304278 FBpp0304734 FBpp0304802 FBpp0311905 FBpp0111655 FBpp0308545 
    ##    8.798192   12.878270   12.267468    9.116675    9.508155   11.262116 
    ## FBpp0310067 FBpp0076328 FBpp0293240 FBpp0070997 FBpp0099836 FBpp0076823 
    ##    8.376540   15.151922    8.958368    8.012751    9.986333   11.997468 
    ## FBpp0308810 FBpp0086727 FBpp0071142 FBpp0293260 FBpp0308603 FBpp0080918 
    ##    4.837943    9.407577    9.066432   10.385353   10.846188    6.980399 
    ## FBpp0072675 FBpp0310232 FBpp0078379 FBpp0110392 FBpp0311091 FBpp0084999 
    ##    8.320042   12.197451    9.569890   10.184898   10.999372   13.471862 
    ## FBpp0084998 FBpp0304477 FBpp0071744 FBpp0302924 FBpp0292491 FBpp0083800 
    ##   14.516862   15.438974    9.924813   11.154807   11.244137    8.362581 
    ## FBpp0077033 FBpp0303527 FBpp0083704 FBpp0306035 FBpp0074937 FBpp0311608 
    ##    8.817042    8.131249    4.599101    9.884615   12.283257    9.268234 
    ## FBpp0080679 FBpp0088169 FBpp0304017 FBpp0100104 FBpp0301108 FBpp0071994 
    ##    9.850386    8.998514    6.889518    9.836624    6.758526    7.359470 
    ## FBpp0084575 FBpp0310702 FBpp0071136 FBpp0290388 FBpp0088377 FBpp0303668 
    ##   10.508749    8.929178    9.378827    9.818175    6.631739    8.584689 
    ## FBpp0084248 FBpp0088518 FBpp0073508 FBpp0290924 FBpp0111603 FBpp0111604 
    ##    8.797956    8.957893    5.220048   11.050368    7.308587    9.115777 
    ## FBpp0289067 FBpp0291053 FBpp0291052 FBpp0306149 FBpp0099909 FBpp0293077 
    ##   10.572218   12.629241   12.681446    8.113363    8.523918    9.099228 
    ## FBpp0312375 FBpp0076470 FBpp0306699 FBpp0306701 FBpp0310302 FBpp0080638 
    ##   11.241149   10.317618    9.805949    8.479192   10.352485    9.707077 
    ## FBpp0301780 FBpp0074009 FBpp0305995 FBpp0070363 FBpp0070364 FBpp0070362 
    ##    6.554031   10.466901   15.175058    8.929339   11.149736    9.035724 
    ## FBpp0084495 FBpp0084494 FBpp0087762 FBpp0113017 FBpp0075761 FBpp0074733 
    ##    9.732984    9.486890   12.236188   11.661389    9.606792    8.572270 
    ## FBpp0291675 FBpp0084776 FBpp0077252 FBpp0075533 FBpp0088417 FBpp0297991 
    ##    8.386921    7.150301    5.932886    7.771549    8.294287    6.382290 
    ## FBpp0291528 FBpp0074845 FBpp0309048 FBpp0077042 FBpp0308589 FBpp0078689 
    ##    6.451061   10.354890   10.542874   10.818335   10.939739   11.830314 
    ## FBpp0289611 FBpp0291030 FBpp0072908 FBpp0074122 FBpp0099826 FBpp0070402 
    ##   11.135639   10.100423   12.496454    5.724452    7.090738    9.031870 
    ## FBpp0310622 FBpp0111786 FBpp0303394 FBpp0080257 FBpp0078326 FBpp0302575 
    ##    9.850414    7.684717    8.613639   10.972928    9.801782    9.620953 
    ## FBpp0110132 FBpp0070862 FBpp0077107 FBpp0290722 FBpp0291457 FBpp0077147 
    ##    9.069267    7.450311    9.330754    8.929178   10.205349   10.022897 
    ## FBpp0292780 FBpp0292781 FBpp0296944 FBpp0296943 FBpp0292316 FBpp0306777 
    ##    7.868233    8.314532   13.565963   12.653334   12.227386    7.898010 
    ## FBpp0079946 FBpp0080387 FBpp0085851 FBpp0086057 FBpp0306074 FBpp0306682 
    ##    8.932832    8.449186   10.265913    8.461963    9.854103   10.212728 
    ## FBpp0307737 FBpp0081476 FBpp0291789 FBpp0303374 FBpp0079912 FBpp0271815 
    ##   10.680136   10.085412   11.170414   10.376875    7.974362   13.698663 
    ## FBpp0087047 FBpp0311524 FBpp0308533 FBpp0294031 FBpp0087979 FBpp0082579 
    ##    6.758768    8.981074    5.945710    5.915608   11.611696    8.462707 
    ## FBpp0081733 FBpp0086104 FBpp0304462 FBpp0297663 FBpp0082593 FBpp0078006 
    ##    9.137969   10.869299   12.131816   14.491538    7.712996    7.870365 
    ## FBpp0073648 FBpp0306724 FBpp0304888 FBpp0304885 FBpp0084731 FBpp0084730 
    ##    8.206348    8.997641   11.741030   12.180079    6.991161   11.804710 
    ## FBpp0087135 FBpp0111944 FBpp0303775 FBpp0311670 FBpp0309872 FBpp0290012 
    ##   11.822209    6.324221   11.878030    9.321190    9.756981    9.527050 
    ## FBpp0300406 FBpp0311421 FBpp0073005 FBpp0073310 FBpp0291479 FBpp0074460 
    ##   10.684828   10.321785    8.176014   10.122357    8.932778    6.620320 
    ## FBpp0291264 FBpp0087366 FBpp0076338 FBpp0294000 FBpp0080114 FBpp0301163 
    ##   11.312161    8.955623    8.634910   10.041983   11.068974    7.740486 
    ## FBpp0084441 FBpp0071672 FBpp0300801 FBpp0309600 FBpp0073474 FBpp0074919 
    ##    9.709554   10.791799   10.552145    9.543489   11.440765    8.588306 
    ## FBpp0074742 FBpp0074708 FBpp0305392 FBpp0081453 FBpp0077427 FBpp0084254 
    ##    9.996512   10.760463    9.526908   12.859725    7.444300    9.439661 
    ## FBpp0309851 FBpp0305604 FBpp0311260 FBpp0305786 FBpp0305785 FBpp0081507 
    ##    6.554589    6.015086   11.030943   10.277478   13.487861    9.541413 
    ## FBpp0083099 FBpp0303771 FBpp0303930 FBpp0303931 FBpp0291072 FBpp0111686 
    ##    4.565154    9.300831   10.443584   10.685450   10.674321    5.277985 
    ## FBpp0304507 FBpp0077174 FBpp0303628 FBpp0076121 FBpp0086049 FBpp0087272 
    ##   11.603382   10.478678   14.045195    8.751240    5.174290    8.506136 
    ## FBpp0306755 FBpp0307450 FBpp0304330 FBpp0080287 FBpp0073391 FBpp0085157 
    ##    9.996101    5.488936    9.101331   10.456112    6.283720    8.285066 
    ## FBpp0290176 FBpp0099488 FBpp0081725 FBpp0077788 FBpp0309407 FBpp0289098 
    ##    9.873332    8.712121    9.187172    9.243563   10.952821    5.674995 
    ## FBpp0086771 FBpp0077121 FBpp0075456 FBpp0075859 FBpp0304907 FBpp0084264 
    ##    9.453682    8.405141   11.658504    9.514696    8.747216    9.923219 
    ## FBpp0312580 FBpp0075790 FBpp0075791 FBpp0304695 FBpp0087277 FBpp0307452 
    ##    9.988091   10.011685    9.982683    7.779809    9.277139    9.694025 
    ## FBpp0087692 FBpp0304736 FBpp0087221 FBpp0085514 FBpp0089175 FBpp0073488 
    ##    6.788970    8.950278   11.066945    8.668111    6.265423    7.339688 
    ## FBpp0312046 FBpp0305549 FBpp0082205 FBpp0304812 FBpp0077617 FBpp0082213 
    ##    8.505559   10.871359   14.297010    9.309910    8.405838    6.622185 
    ## FBpp0304650 FBpp0086665 FBpp0077909 FBpp0304054 FBpp0304053 FBpp0304049 
    ##    6.142710   10.321354    8.393020    6.356912    8.972379    8.790156 
    ## FBpp0288814 FBpp0072673 FBpp0084099 FBpp0083452 FBpp0288456 FBpp0311069 
    ##    6.210162    9.178461   11.393478   10.760780   11.029852   10.030592 
    ## FBpp0082540 FBpp0304623 FBpp0072825 FBpp0112050 FBpp0086084 FBpp0084165 
    ##    7.802898    7.187847    7.302286    9.443358    8.465084    9.139923 
    ## FBpp0293620 FBpp0292197 FBpp0290456 FBpp0310597 FBpp0308219 FBpp0073267 
    ##   11.346141    7.841493    8.829953   10.603390    9.026096    8.602210 
    ## FBpp0311871 FBpp0305239 FBpp0309465 FBpp0311517 FBpp0086373 FBpp0304067 
    ##   11.846630    7.947411   10.391439    8.586942   11.217473   10.844905 
    ## FBpp0305757 FBpp0303193 FBpp0087986 FBpp0082270 FBpp0077204 FBpp0290837 
    ##   11.766280   10.731921    9.746972   11.977709    9.514409   10.436163 
    ## FBpp0087719 FBpp0087720 FBpp0087718 FBpp0293114 FBpp0087721 FBpp0300426 
    ##   10.669384   10.804498    6.928370    5.633854   10.371935   10.904498 
    ## FBpp0307227 FBpp0078598 FBpp0308990 FBpp0074616 FBpp0075523 FBpp0309601 
    ##    7.961160   10.321703   12.151345    7.647655    9.874171   10.829809 
    ## FBpp0086020 FBpp0309015 FBpp0070160 FBpp0077962 FBpp0073100 FBpp0099998 
    ##   10.199672    7.602615    8.235536   11.607372   10.028760   10.764872 
    ## FBpp0074400 FBpp0076201 FBpp0087368 FBpp0100067 FBpp0086958 FBpp0073438 
    ##    8.738031    7.812265    9.151455    6.256873    8.981853   10.669835 
    ## FBpp0312146 FBpp0070411 FBpp0307743 FBpp0072703 FBpp0311281 FBpp0082383 
    ##    9.716523    6.365527   11.783192   10.081157   13.785073    9.243001 
    ## FBpp0077054 FBpp0310632 FBpp0075632 FBpp0290049 FBpp0083070 FBpp0083673 
    ##   12.589647    7.833925   11.840635    6.810163    9.238882    8.924759 
    ## FBpp0312104 FBpp0307598 FBpp0080003 FBpp0087481 FBpp0301728 FBpp0307411 
    ##   13.356374    7.840921   10.347561    8.478751   10.931436   10.793323 
    ## FBpp0072077 FBpp0297347 FBpp0089315 FBpp0305278 FBpp0306125 FBpp0085707 
    ##   10.163558   13.314048    9.317289    8.194667    8.872381   10.063934 
    ## FBpp0310802 FBpp0079631 FBpp0298308 FBpp0087774 FBpp0080016 FBpp0075169 
    ##    8.968038    9.869944   12.418615    9.022872    9.911065    7.600183 
    ## FBpp0304267 FBpp0297286 FBpp0304213 FBpp0075700 FBpp0309730 FBpp0080701 
    ##    8.879750    8.548647    7.625244    5.882643   10.140249    8.497017 
    ## FBpp0306553 FBpp0304855 FBpp0304854 FBpp0310015 FBpp0072022 FBpp0081737 
    ##   11.298427    7.759253    8.827819    9.654831    9.101522    7.910057 
    ## FBpp0072540 FBpp0303106 FBpp0308767 FBpp0309884 FBpp0305671 FBpp0076094 
    ##    5.695893    6.716742    6.704470    7.669594   10.894377    6.550957 
    ## FBpp0312515 FBpp0080022 FBpp0071222 FBpp0292592 FBpp0080049 FBpp0082865 
    ##    8.981126    9.041360    8.512974    9.547771   10.732090    8.736740 
    ## FBpp0070172 FBpp0304575 FBpp0297275 FBpp0077623 FBpp0088430 FBpp0086848 
    ##   11.356072    7.771549    7.314367    8.134520    8.840578    9.408233 
    ## FBpp0071583 FBpp0291372 FBpp0070998 FBpp0294038 FBpp0073906 FBpp0305942 
    ##    8.529253   13.515422    8.398783   11.391078    8.832948   11.233903 
    ## FBpp0073890 FBpp0293606 FBpp0083026 FBpp0071470 FBpp0303745 FBpp0081331 
    ##    8.164998    7.594494    7.831278   11.225448    8.131717    7.643463 
    ## FBpp0081330 FBpp0289146 FBpp0303963 FBpp0079220 FBpp0311688 FBpp0305479 
    ##   12.148494   10.218970    7.998154    9.655612   11.409892   10.747400 
    ## FBpp0071376 FBpp0070644 FBpp0085953 FBpp0113073 FBpp0070907 FBpp0083898 
    ##    8.667918    7.854358   10.192382    8.904648    8.256788   12.454486 
    ## FBpp0083899 FBpp0072839 FBpp0311204 FBpp0290407 FBpp0077678 FBpp0306037 
    ##   11.978509   14.661687   12.217495    9.616348    7.678328   10.153345 
    ## FBpp0304606 FBpp0305783 FBpp0310086 FBpp0300565 FBpp0306444 FBpp0311744 
    ##    9.954751    9.930173   11.230468   11.075260   10.962673   10.315849 
    ## FBpp0113092 FBpp0305470 FBpp0074709 FBpp0309305 FBpp0083174 FBpp0304992 
    ##    8.928209    6.462112    9.321190    8.177737    9.162941    5.922111 
    ## FBpp0078139 FBpp0303007 FBpp0305433 FBpp0271920 FBpp0085394 FBpp0289731 
    ##   10.617651   11.733755    7.623250    7.656002    8.376145    8.539229 
    ## FBpp0290630 FBpp0073538 FBpp0078414 FBpp0309926 FBpp0089002 FBpp0100043 
    ##   11.825898   10.126094    9.134846   10.876587    7.813898   12.089733 
    ## FBpp0082727 FBpp0070415 FBpp0080943 FBpp0085658 FBpp0075646 FBpp0077132 
    ##    9.348286    8.167555   10.475291   11.160742   10.227660    9.079921 
    ## FBpp0082468 FBpp0311863 FBpp0302568 FBpp0071811 FBpp0082539 FBpp0111711 
    ##    8.757557    8.684780   10.494837   10.386881   11.623225   11.940053 
    ## FBpp0297439 FBpp0297438 FBpp0307469 FBpp0088886 FBpp0079093 FBpp0073835 
    ##   11.350939   11.942481    8.741835   11.548393    7.793470    9.620386 
    ## FBpp0306600 FBpp0112215 FBpp0310241 FBpp0310184 FBpp0111975 FBpp0087487 
    ##    9.279041    9.477832    9.705475    9.746056    9.303864    7.926862 
    ## FBpp0070637 FBpp0301756 FBpp0289650 FBpp0309333 FBpp0311785 FBpp0304678 
    ##    9.949668    8.575367    7.487621    9.945151    9.317906    6.678072 
    ## FBpp0304676 FBpp0110232 FBpp0078457 FBpp0078454 FBpp0111520 FBpp0111521 
    ##    7.333521   11.152423   10.000461   11.312914    7.618853    7.895479 
    ## FBpp0309018 FBpp0077714 FBpp0290558 FBpp0084110 FBpp0085121 FBpp0071235 
    ##    7.464193    9.480698   11.280075    9.809432    9.079048   10.112179 
    ## FBpp0311987 FBpp0112471 FBpp0302837 FBpp0083397 FBpp0110120 FBpp0090943 
    ##   15.329484   11.509586    6.242395   10.703542    9.726187   10.166893 
    ## FBpp0311184 FBpp0079294 FBpp0312522 FBpp0082044 FBpp0305767 FBpp0289118 
    ##    9.645102    8.859507    7.984341   10.986256    8.538665    8.497961 
    ## FBpp0305931 FBpp0311859 FBpp0304807 FBpp0293948 FBpp0309949 FBpp0112375 
    ##   10.856666    9.865621    8.051629   11.149828    9.676310    6.857981 
    ## FBpp0310726 FBpp0303765 FBpp0297250 FBpp0078399 FBpp0308728 FBpp0308653 
    ##    7.401655    9.016251    8.567852   13.374296    9.579179   10.897777 
    ## FBpp0303635 FBpp0079437 FBpp0085372 FBpp0306395 FBpp0083022 FBpp0293159 
    ##    9.502814   13.084968    7.460921   11.289736    8.719731   10.348788 
    ## FBpp0293161 FBpp0311289 FBpp0293162 FBpp0071848 FBpp0070298 FBpp0083948 
    ##   10.352043    9.079000    9.099659   10.110519   11.035174   12.393488 
    ## FBpp0306961 FBpp0077841 FBpp0301955 FBpp0307022 FBpp0296971 FBpp0075027 
    ##   10.052642    9.074774   10.727317    8.990697    8.525628   11.101641 
    ## FBpp0308602 FBpp0078405 FBpp0077492 FBpp0076451 FBpp0078565 FBpp0080819 
    ##    6.728910    9.189914    9.329368    9.609817    8.243174    5.694374 
    ## FBpp0303977 FBpp0310135 FBpp0086328 FBpp0073767 FBpp0070113 FBpp0087515 
    ##    5.982683    9.376974   12.777812    8.210516    8.477427   10.600775 
    ## FBpp0082982 FBpp0310166 FBpp0290024 FBpp0300430 FBpp0300414 FBpp0070482 
    ##    8.941074   11.681893    7.806301    8.079921   12.226511    8.208212 
    ## FBpp0080448 FBpp0075644 FBpp0289778 FBpp0293231 FBpp0077890 FBpp0304246 
    ##    8.834040    7.850386    8.702834    7.236319    7.327491    7.115351 
    ## FBpp0074730 FBpp0075538 FBpp0305182 FBpp0084016 FBpp0071571 FBpp0071570 
    ##    6.698168   10.151616   16.214826   11.935004   12.008932   11.638666 
    ## FBpp0304765 FBpp0110241 FBpp0091112 FBpp0084812 FBpp0312237 FBpp0311354 
    ##    9.886639   11.627103    4.682165   10.564635   11.347601    9.421835 
    ## FBpp0309268 FBpp0071629 FBpp0079568 FBpp0307849 FBpp0297078 FBpp0081074 
    ##    6.781598   10.097649   11.407084    9.672265   11.624388   10.650040 
    ## FBpp0083512 FBpp0310305 FBpp0310304 FBpp0075238 FBpp0308840 FBpp0078012 
    ##    5.927293    8.216084    9.991779   11.066725    7.689426    7.821687 
    ## FBpp0304959 FBpp0075212 FBpp0080595 FBpp0297152 FBpp0071168 FBpp0310508 
    ##   11.200330   10.738599    8.885253   10.005191    7.445203    8.820469 
    ## FBpp0082590 FBpp0088146 FBpp0291325 FBpp0087222 FBpp0087733 FBpp0099713 
    ##    7.060794    8.672490   13.799315    4.879528   10.236275    8.776583 
    ## FBpp0111811 FBpp0305723 FBpp0086223 FBpp0072931 FBpp0070054 FBpp0309712 
    ##    7.126282    7.429464    9.092517    8.771009    9.191530    9.652584 
    ## FBpp0307880 FBpp0307664 FBpp0307797 FBpp0311695 FBpp0081828 FBpp0077085 
    ##    7.269849    7.136012   10.912590    8.547385   10.013944   10.903170 
    ## FBpp0082685 FBpp0082686 FBpp0298359 FBpp0302585 FBpp0311357 FBpp0088628 
    ##   11.720983   11.730663    9.870953   12.808376   12.390970    9.253826 
    ## FBpp0088627 FBpp0081980 FBpp0074972 FBpp0305446 FBpp0083501 FBpp0305410 
    ##   10.253998    9.053556   10.972640    8.276801   10.406920    8.672875 
    ## FBpp0111757 FBpp0305411 FBpp0086704 FBpp0078120 FBpp0305015 FBpp0305016 
    ##    9.023931    9.023679    8.591097    9.384175    8.738031   10.832789 
    ## FBpp0289642 FBpp0296980 FBpp0072618 FBpp0072620 FBpp0306964 FBpp0074348 
    ##   10.373972   11.118788    7.715744    5.333033    8.986049   11.561262 
    ## FBpp0085137 FBpp0072323 FBpp0078315 FBpp0301208 FBpp0090963 FBpp0308828 
    ##    8.680439   10.086402    8.093718    7.776822    8.535982    7.192696 
    ## FBpp0077396 FBpp0304567 FBpp0089095 FBpp0310306 FBpp0310459 FBpp0300317 
    ##    8.750265    8.706731    6.443848   10.100304    9.672168   11.581055 
    ## FBpp0084623 FBpp0075318 FBpp0075319 FBpp0291544 FBpp0304250 FBpp0084240 
    ##   13.574110   10.459283   10.324651    6.990336    8.434477   11.160479 
    ## FBpp0307707 FBpp0311006 FBpp0083850 FBpp0293271 FBpp0071945 FBpp0086203 
    ##    8.562381    7.855152   10.056441   10.083286   10.156910    8.819482 
    ## FBpp0311640 FBpp0310889 FBpp0308775 FBpp0308774 FBpp0082287 FBpp0289769 
    ##   13.972653   11.019957    9.141736    9.754523    4.048363    6.864299 
    ## FBpp0078887 FBpp0303137 FBpp0292214 FBpp0076112 FBpp0303404 FBpp0083632 
    ##    8.339445    9.615780   11.949097    9.817100    9.591981    9.625775 
    ## FBpp0301099 FBpp0301949 FBpp0306969 FBpp0303328 FBpp0298338 FBpp0086101 
    ##   12.613166   12.472958   10.316981   11.940641    5.428703    9.242178 
    ## FBpp0304370 FBpp0289092 FBpp0079045 FBpp0086969 FBpp0088857 FBpp0088858 
    ##    6.297458    8.047669    8.934012    8.232486   10.858009   10.861242 
    ## FBpp0291582 FBpp0112323 FBpp0305229 FBpp0307247 FBpp0308789 FBpp0086980 
    ##   10.177080    8.491051    9.477170    7.788021   10.578338    9.356512 
    ## FBpp0301154 FBpp0309937 FBpp0087722 FBpp0076252 FBpp0297429 FBpp0078636 
    ##   10.575023   10.234926   10.353688    9.145168    9.540075    9.860212 
    ## FBpp0290430 FBpp0290429 FBpp0077028 FBpp0111469 FBpp0074481 FBpp0085545 
    ##    6.547876    7.955254   13.118838   10.050986   10.014097    9.575195 
    ## FBpp0086181 FBpp0086182 FBpp0071810 FBpp0072893 FBpp0072117 FBpp0079493 
    ##    9.860156    9.824207    7.318152    9.919568    9.971152    5.441435 
    ## FBpp0087864 FBpp0307775 FBpp0089026 FBpp0089025 FBpp0311942 FBpp0074414 
    ##   10.133119   10.866380   10.617718   11.007754    9.537783    2.045189 
    ## FBpp0072593 FBpp0310538 FBpp0308780 FBpp0084349 FBpp0079586 FBpp0309518 
    ##   10.179208    9.852005   10.307222    9.835821   11.464936    8.582022 
    ## FBpp0075612 FBpp0079885 FBpp0079886 FBpp0088565 FBpp0076237 FBpp0081216 
    ##   14.040736    8.938136    9.918565   12.463092   11.026171   10.159894 
    ## FBpp0303947 FBpp0293574 FBpp0083595 FBpp0311704 FBpp0081863 FBpp0088926 
    ##   11.399744    7.494328    9.183071   11.012180    9.275447   15.344049 
    ## FBpp0088452 FBpp0084561 FBpp0305476 FBpp0271885 FBpp0293167 FBpp0310058 
    ##   14.857488    7.600724    8.655287    8.502090    9.652421   13.562464 
    ## FBpp0070317 FBpp0290995 FBpp0271898 FBpp0087065 FBpp0304111 FBpp0304752 
    ##    9.124356    9.194757   10.405432   10.666014    8.097170   10.165341 
    ## FBpp0304980 FBpp0072423 FBpp0306941 FBpp0089380 FBpp0088152 FBpp0088153 
    ##    9.959449    9.175652    9.598898    7.990130   11.453037   11.494810 
    ## FBpp0310844 FBpp0078478 FBpp0310307 FBpp0297611 FBpp0076153 FBpp0309532 
    ##    5.752457    9.531062   11.991792    9.991522   14.299401   14.408910 
    ## FBpp0290743 FBpp0077354 FBpp0310842 FBpp0084788 FBpp0113077 FBpp0309776 
    ##    5.520708    6.213701   11.819736    7.380087    9.328389    7.266105 
    ## FBpp0087006 FBpp0086387 FBpp0079732 FBpp0087251 FBpp0085961 FBpp0082547 
    ##    8.763665    8.518850   10.315952    9.291820   10.581920    9.292112 
    ## FBpp0086663 FBpp0297292 FBpp0311797 FBpp0073557 FBpp0305432 FBpp0305431 
    ##    9.191889    9.090161   10.102405    9.289474   11.042356   10.734448 
    ## FBpp0072930 FBpp0073400 FBpp0289693 FBpp0080887 FBpp0086205 FBpp0083195 
    ##    7.684334   10.397850   10.241897    3.621652    8.068925   10.574490 
    ## FBpp0080886 FBpp0308361 FBpp0086435 FBpp0099793 FBpp0307859 FBpp0303864 
    ##    4.733354    9.096739    9.508623   11.167897   11.419117   10.169287 
    ## FBpp0077122 FBpp0112349 FBpp0307592 FBpp0112984 FBpp0311763 FBpp0290588 
    ##    9.513584   10.879291    9.436030   10.553315   11.238676   12.486364 
    ## FBpp0303266 FBpp0291327 FBpp0084241 FBpp0080755 FBpp0308859 FBpp0300986 
    ##   13.755459   11.030680   10.058336    7.868233    8.836337    7.856624 
    ## FBpp0290880 FBpp0088686 FBpp0088685 FBpp0289803 FBpp0304195 FBpp0110174 
    ##    7.271718   10.317186    5.750022    5.528685   11.538145   10.437468 
    ## FBpp0076837 FBpp0076647 FBpp0300836 FBpp0100080 FBpp0079218 FBpp0078361 
    ##   14.891301   11.221807   10.920840   11.160399    9.690188    7.457940 
    ## FBpp0077144 FBpp0082438 FBpp0085716 FBpp0074609 FBpp0071921 FBpp0082225 
    ##   10.562503    8.565223   10.540269    9.975509    9.046082   11.026473 
    ## FBpp0310568 FBpp0303669 FBpp0086186 FBpp0303034 FBpp0087376 FBpp0087375 
    ##    8.383547    9.316179    9.108239   10.040912    9.060057    9.073022 
    ## FBpp0298350 FBpp0075317 FBpp0072660 FBpp0297922 FBpp0077335 FBpp0302002 
    ##    3.958421    9.669078   10.802810    4.857981    5.685736    7.469975 
    ## FBpp0271693 FBpp0312512 FBpp0303188 FBpp0074251 FBpp0089088 FBpp0077103 
    ##    9.446143   11.029864    9.565223   10.247582   13.604724    8.198512 
    ## FBpp0310656 FBpp0086603 FBpp0080564 FBpp0303613 FBpp0090953 FBpp0306723 
    ##   10.262266   12.061261   10.506389    9.488315   10.318892   10.426493 
    ## FBpp0090952 FBpp0083248 FBpp0072854 FBpp0303172 FBpp0288784 FBpp0306142 
    ##   11.987451   12.103908    7.050343   10.683234   13.270200    5.146418 
    ## FBpp0311343 FBpp0309942 FBpp0297229 FBpp0078463 FBpp0293292 FBpp0304791 
    ##   10.233249    8.917019    8.567161   12.671690    9.342034    8.834040 
    ## FBpp0306684 FBpp0293338 FBpp0305213 FBpp0304655 FBpp0309618 FBpp0305074 
    ##   10.554537    8.321108    8.829780    6.996101    9.316302    9.810659 
    ## FBpp0309171 FBpp0290490 FBpp0070953 FBpp0077314 FBpp0079090 FBpp0290511 
    ##    9.996486   10.215732   10.430015   11.097517    8.335796    7.200743 
    ## FBpp0085224 FBpp0077934 FBpp0306799 FBpp0078343 FBpp0082596 FBpp0087770 
    ##    8.911528   10.212618   10.495037    9.091508   12.853345    5.455252 
    ## FBpp0291059 FBpp0289521 FBpp0300815 FBpp0111666 FBpp0304349 FBpp0311618 
    ##   10.318461   10.319939    8.020551    9.259572   10.240011    8.291192 
    ## FBpp0289959 FBpp0291628 FBpp0293235 FBpp0304386 FBpp0290275 FBpp0089344 
    ##    3.837026    6.637284   11.040775    7.955254    7.028609    5.932886 
    ## FBpp0309392 FBpp0110110 FBpp0289288 FBpp0311906 FBpp0112293 FBpp0112292 
    ##   11.523446   10.075090    8.379221   11.119189    8.064326   10.190229 
    ## FBpp0310349 FBpp0076099 FBpp0307929 FBpp0071478 FBpp0292511 FBpp0303451 
    ##    8.606321    7.945497    6.783028   10.752198   11.907819   10.246913 
    ## FBpp0073084 FBpp0073627 FBpp0077836 FBpp0304201 FBpp0074107 FBpp0310364 
    ##    7.446707    8.896195   12.025247   11.450780    9.305067    7.444902 
    ## FBpp0080011 FBpp0076343 FBpp0305262 FBpp0075645 FBpp0290798 FBpp0293213 
    ##    5.312718   10.123698    7.198423    8.501005    8.789089    9.884642 
    ## FBpp0078625 FBpp0312080 FBpp0303072 FBpp0306751 FBpp0085281 FBpp0293600 
    ##    6.426875   15.155899    8.824525    8.848168    2.976864    9.336162 
    ## FBpp0099843 FBpp0310025 FBpp0305548 FBpp0071593 FBpp0111700 FBpp0306893 
    ##   12.202135   10.442642    7.610555   10.347621    9.488753    8.891121 
    ## FBpp0071469 FBpp0081027 FBpp0086629 FBpp0303879 FBpp0303082 FBpp0072072 
    ##    4.855265    9.363179   10.287041    7.872829    8.255329   10.800812 
    ## FBpp0079589 FBpp0307453 FBpp0077339 FBpp0293270 FBpp0304988 FBpp0301574 
    ##    8.848395    7.725444    4.384803    8.388565   11.836481    4.578390 
    ## FBpp0081989 FBpp0304694 FBpp0305426 FBpp0307147 FBpp0082591 FBpp0080826 
    ##    9.459469    9.333643    9.006288    8.086788    9.049205    9.350015 
    ## FBpp0310843 FBpp0303944 FBpp0083249 FBpp0303595 FBpp0294023 FBpp0294024 
    ##   13.757892    9.639357    5.286075    3.910384    9.565743    9.690347 
    ## FBpp0077911 FBpp0080700 FBpp0306780 FBpp0312224 FBpp0099770 FBpp0071316 
    ##    9.510890    8.972954    9.683824   14.863570   10.593968    7.955888 
    ## FBpp0304066 FBpp0078756 FBpp0312149 FBpp0083656 FBpp0271854 FBpp0084161 
    ##   12.872829   11.882824   10.817725   12.893722    7.879193   12.589179 
    ## FBpp0083373 FBpp0083975 FBpp0078663 FBpp0082329 FBpp0086786 FBpp0070517 
    ##   10.438055    8.250686   11.047496    7.088427    9.576948    3.829665 
    ## FBpp0291631 FBpp0085353 FBpp0085351 FBpp0079614 FBpp0078265 FBpp0075168 
    ##    6.929662    9.164450    9.182485    8.022267   11.473263   11.482927 
    ## FBpp0081602 FBpp0074055 FBpp0075707 FBpp0303809 FBpp0086322 FBpp0074017 
    ##    8.079824   10.309187   14.150588   10.942261    9.807940   14.367500 
    ## FBpp0307742 FBpp0307741 FBpp0310028 FBpp0075837 FBpp0289271 FBpp0071530 
    ##   10.415031    8.502741   12.623337    4.821339    7.734833   10.876740 
    ## FBpp0305284 FBpp0087196 FBpp0087534 FBpp0081209 FBpp0073009 FBpp0308705 
    ##   11.941988   10.004629   12.255211    8.568129    8.415780    9.924273 
    ## FBpp0083503 FBpp0080449 FBpp0078376 FBpp0072135 FBpp0082599 FBpp0073293 
    ##    8.607936   12.389250   11.540366   11.357222    4.228469    7.919730 
    ## FBpp0291316 FBpp0080889 FBpp0305150 FBpp0297427 FBpp0076695 FBpp0304366 
    ##    9.025140   10.003020    6.370291    9.920732    8.839776   12.944010 
    ## FBpp0302767 FBpp0073562 FBpp0303477 FBpp0303476 FBpp0088396 FBpp0110350 
    ##   12.102864   11.867531    7.323075    6.911038   13.523789    6.488351 
    ## FBpp0110346 FBpp0297695 FBpp0110394 FBpp0088091 FBpp0288481 FBpp0305289 
    ##    7.448210    6.278998    6.221455    7.484255    7.164816   10.384666 
    ## FBpp0071505 FBpp0071507 FBpp0290663 FBpp0305367 FBpp0306714 FBpp0306430 
    ##    7.190184    8.422447   12.279436    2.891066   10.213679    9.420152 
    ## FBpp0071509 FBpp0302593 FBpp0077167 FBpp0304593 FBpp0308267 FBpp0304638 
    ##   10.960937    8.109664   11.508110   13.948213   11.942055   11.682835 
    ## FBpp0304642 FBpp0309276 FBpp0306903 FBpp0294020 FBpp0087126 FBpp0301732 
    ##   11.535629    9.403012   11.997680   10.557359    7.984237   10.795789 
    ## FBpp0292147 FBpp0307963 FBpp0303370 FBpp0111712 FBpp0070791 FBpp0086110 
    ##    9.751240    6.444751   10.324712    9.643659    7.709491   11.291663 
    ## FBpp0085449 FBpp0311537 FBpp0074717 FBpp0310174 FBpp0298370 FBpp0310631 
    ##    6.417085   11.965771    7.764691    9.255329   13.212789    9.207059 
    ## FBpp0087347 FBpp0072151 FBpp0080521 FBpp0312423 FBpp0079964 FBpp0073643 
    ##    8.152700   11.233282    9.852771    4.529253    5.038021    4.327165 
    ## FBpp0306704 FBpp0309257 FBpp0099426 FBpp0303989 FBpp0303990 FBpp0309988 
    ##    5.143453   10.564860    8.547315    7.629092    8.145214    8.201634 
    ## FBpp0304594 FBpp0079702 FBpp0079183 FBpp0086904 FBpp0311996 FBpp0080774 
    ##    9.776373    6.666498   10.223914    9.677816    4.654311    4.569579 
    ## FBpp0072016 FBpp0079780 FBpp0083678 FBpp0073016 FBpp0297442 FBpp0305318 
    ##    5.654831    9.921895    8.680823    9.779421   11.344447   13.033251 
    ## FBpp0303571 FBpp0113010 FBpp0307736 FBpp0307747 FBpp0311844 FBpp0083168 
    ##    8.736064    7.686882    9.419194    7.891508    9.494619    8.323239 
    ## FBpp0312381 FBpp0306611 FBpp0310682 FBpp0076363 FBpp0311505 FBpp0311562 
    ##    9.260984   12.053494   10.914154   10.673791    6.666757    9.970184 
    ## FBpp0304270 FBpp0305302 FBpp0305303 FBpp0080408 FBpp0080256 FBpp0297243 
    ##    9.802164   12.242995   11.400491    9.048561    8.388487    9.690601 
    ## FBpp0082250 FBpp0306911 FBpp0312031 FBpp0303895 FBpp0100187 FBpp0087031 
    ##    7.205193   11.358461   11.123087    4.745140   15.030738    8.445730 
    ## FBpp0309567 FBpp0076183 FBpp0082767 FBpp0071259 FBpp0306412 FBpp0086643 
    ##   10.028233    9.650171    8.511537    9.233445   10.793278    9.699588 
    ## FBpp0306740 FBpp0073750 FBpp0308667 FBpp0303465 FBpp0077210 FBpp0306846 
    ##   11.903457    8.389035    8.345829    9.272651    9.955835    9.276590 
    ## FBpp0310039 FBpp0081592 FBpp0082068 FBpp0079399 FBpp0309930 FBpp0084162 
    ##    9.816867   10.901621    7.253439    7.647131   10.750265    8.706731 
    ## FBpp0303849 FBpp0310826 FBpp0088080 FBpp0310275 FBpp0074825 FBpp0071732 
    ##    9.385666   10.217583   10.653692    4.045189   12.562628    6.051135 
    ## FBpp0086096 FBpp0083371 FBpp0078319 FBpp0312482 FBpp0087236 FBpp0080532 
    ##    9.057450   14.176710    8.953667    5.413397    6.150117    9.661487 
    ## FBpp0306146 FBpp0071275 FBpp0071892 FBpp0082867 FBpp0081659 FBpp0304538 
    ##    4.038819    9.367752   11.438159   10.428113   11.723925   12.650256 
    ## FBpp0307011 FBpp0082996 FBpp0089177 FBpp0071940 FBpp0303003 FBpp0075833 
    ##    9.798192    9.295247    7.222158   10.743626   10.205260    8.309910 
    ## FBpp0300826 FBpp0303827 FBpp0306442 FBpp0308324 FBpp0085917 FBpp0305067 
    ##    8.935995   11.389514   12.322025   13.892577   16.135541   11.264741 
    ## FBpp0306023 FBpp0305158 FBpp0307150 FBpp0305520 FBpp0076459 FBpp0085065 
    ##    8.831682    7.095733    4.650138   11.413830    6.698926   10.530264 
    ## FBpp0082121 FBpp0088021 FBpp0086098 FBpp0084782 FBpp0084842 FBpp0082984 
    ##   10.470677   11.368149   13.044115    7.418773    9.161613    9.383115 
    ## FBpp0087647 FBpp0099972 FBpp0080622 FBpp0087092 FBpp0075466 FBpp0290948 
    ##    8.377171   11.602741    9.521422    8.523348    7.009396    8.229431 
    ## FBpp0078894 FBpp0303176 FBpp0305308 FBpp0071145 FBpp0084989 FBpp0070143 
    ##    7.614977    7.919405   10.171177    7.234926    9.046132   14.091904 
    ## FBpp0073458 FBpp0085430 FBpp0084172 FBpp0112128 FBpp0293236 FBpp0305603 
    ##    9.927158    9.640804    7.616850    7.456896   12.795184   12.209431 
    ## FBpp0291704 FBpp0072083 FBpp0307562 FBpp0298351 FBpp0072518 FBpp0084050 
    ##   11.959343   10.871639    9.674032   11.145295    8.458388    8.087270 
    ## FBpp0080659 FBpp0307999 FBpp0304005 FBpp0078929 FBpp0079470 FBpp0074092 
    ##   12.072675    5.550677   10.429996    8.881309    7.822151    5.635438 
    ## FBpp0305562 FBpp0081390 FBpp0076723 FBpp0089108 FBpp0085122 FBpp0099934 
    ##   14.220318    6.268149    8.882310    6.269169    7.196457   13.406265 
    ## FBpp0071897 FBpp0072129 FBpp0304566 FBpp0309606 FBpp0079233 FBpp0086627 
    ##    8.839948    7.612701   10.763137   11.446557   11.308711   12.170181 
    ## FBpp0301157 FBpp0307729 FBpp0100180 FBpp0071847 FBpp0111920 FBpp0070860 
    ##   10.460568    8.827299   15.627872    9.722311   11.971948    8.633524 
    ## FBpp0087118 FBpp0079495 FBpp0087367 FBpp0304236 FBpp0084959 FBpp0087013 
    ##    8.829088   12.571666   10.976864    9.614676   14.262441    9.250212 
    ## FBpp0307127 FBpp0289480 FBpp0309705 FBpp0083124 FBpp0310904 FBpp0290000 
    ##   10.319015    9.416586   11.112712    8.599778    7.096500    9.894735 
    ## FBpp0112156 FBpp0289181 FBpp0084950 FBpp0070817 FBpp0086701 FBpp0072144 
    ##    6.499991    8.460326   11.830544    5.367752   14.908410    9.975066 
    ## FBpp0310558 FBpp0074662 FBpp0311889 FBpp0071223 FBpp0305858 FBpp0079979 
    ##   10.007970   12.151997    7.938457    7.881197   14.150154   10.180316 
    ## FBpp0311474 FBpp0087346 FBpp0310165 FBpp0306592 FBpp0306426 FBpp0087463 
    ##   12.129447    8.720477    9.523491    7.384175   10.337338    8.081084 
    ## FBpp0303030 FBpp0087870 FBpp0305141 FBpp0291643 FBpp0310943 FBpp0085875 
    ##    9.784962   11.889338   11.406949    9.259957    8.904429    8.325530 
    ## FBpp0074191 FBpp0310331 FBpp0083687 FBpp0076686 FBpp0307934 FBpp0099679 
    ##    8.785765    8.596325    5.935888    8.302452    7.736187    9.335269 
    ## FBpp0311983 FBpp0297140 FBpp0070703 FBpp0074863 FBpp0308792 FBpp0070295 
    ##   11.189330    9.745201    6.791577    7.629754    8.241702    7.847883 
    ## FBpp0083415 FBpp0077308 FBpp0075970 FBpp0084774 FBpp0079375 FBpp0311396 
    ##    9.988349   12.085032    7.298792   11.923476   10.272036    9.222949 
    ## FBpp0311481 FBpp0310769 FBpp0304189 FBpp0072941 FBpp0083843 FBpp0079324 
    ##   13.119955    7.170836   10.244104    9.393878   11.297635   10.624197 
    ## FBpp0298273 FBpp0073316 FBpp0081245 FBpp0312000 FBpp0302563 FBpp0305750 
    ##    6.357232    9.158679    8.737355   10.884157    8.337095    8.876517 
    ## FBpp0077735 FBpp0085690 FBpp0113033 FBpp0072463 FBpp0082735 FBpp0079219 
    ##    9.410625   10.745491    5.533224    8.036623   11.311707    9.379654 
    ## FBpp0300512 FBpp0087734 FBpp0070716 FBpp0311458 FBpp0306837 FBpp0081350 
    ##   14.004066    8.751179    9.299000   14.237645   14.340731    7.575367 
    ## FBpp0077145 FBpp0308582 FBpp0083962 FBpp0086468 FBpp0089414 FBpp0072691 
    ##    7.991676    8.775805    7.788733   11.387371    7.016302   10.631739 
    ## FBpp0075349 FBpp0071497 FBpp0078246 FBpp0290448 FBpp0080120 FBpp0081371 
    ##   10.808057   10.385274   16.655732    9.959291    6.855944   11.848552 
    ## FBpp0309989 FBpp0292398 FBpp0306090 FBpp0309009 FBpp0292508 FBpp0078447 
    ##    9.653301   11.479835    5.443848    2.750022    7.787071   10.563352 
    ## FBpp0083799 FBpp0305395 FBpp0070476 FBpp0082137 FBpp0071748 FBpp0099824 
    ##   10.070536    8.477206    7.894708    9.215908   11.300904   13.617868 
    ## FBpp0305495 FBpp0073430 FBpp0311691 FBpp0312315 FBpp0071846 FBpp0290696 
    ##    8.378196   13.544552   11.608045    8.437279   14.379524    9.605951 
    ## FBpp0310390 FBpp0311384 FBpp0080628 FBpp0070949 FBpp0074909 FBpp0085902 
    ##   10.969151    9.611696    6.982268    8.674224   10.534939   10.203392 
    ## FBpp0077538 FBpp0311526 FBpp0083928 FBpp0305267 FBpp0071451 FBpp0291478 
    ##    8.510171    7.917345   10.889034   11.360428   10.985790    9.753551 
    ## FBpp0308496 FBpp0080648 FBpp0302815 FBpp0087969 FBpp0079041 FBpp0309738 
    ##   11.879075    9.852487    7.228469   11.077471    9.778436   11.543559 
    ## FBpp0312179 FBpp0300667 FBpp0301600 FBpp0309234 FBpp0309201 FBpp0309477 
    ##    7.419232   10.619553   11.646731   11.465900   14.446217   11.885683 
    ## FBpp0310207 FBpp0306002 FBpp0077806 FBpp0083451 FBpp0086465 FBpp0306603 
    ##    9.737447    9.880224    7.459730   10.142061   10.239946   11.929036 
    ## FBpp0303937 FBpp0076098 FBpp0305777 FBpp0087957 FBpp0085166 FBpp0084911 
    ##    9.032021    9.110614    8.478972    6.558490   15.107923   10.799134 
    ## FBpp0081867 FBpp0305959 FBpp0311265 FBpp0085260 FBpp0075034 FBpp0113056 
    ##    6.220400    8.918484    6.761672    9.776612   10.575333    8.704029 
    ## FBpp0305334 FBpp0081860 FBpp0310433 FBpp0084528 FBpp0075382 FBpp0312205 
    ##   10.929232    6.060991   10.689331    8.956944   11.168946   11.850158 
    ## FBpp0306036 FBpp0087859 FBpp0308731 FBpp0076545 FBpp0304646 FBpp0304645 
    ##    9.446933   10.239447   10.439661    7.357232   10.567351   11.772090 
    ## FBpp0305717 FBpp0086269 FBpp0304381 FBpp0312542 FBpp0307760 FBpp0289972 
    ##   13.701381   14.188472    5.879528    4.558490    9.482936    9.409738 
    ## FBpp0305836 FBpp0311779 FBpp0081879 FBpp0072146 FBpp0087241 FBpp0302861 
    ##   10.027227   13.147127   10.330041    8.716430   11.120839    8.170380 
    ## FBpp0306868 FBpp0311461 FBpp0112463 FBpp0085703 FBpp0080335 FBpp0087227 
    ##   10.248661   12.648496    8.128065   12.095301    9.398316    9.615981 
    ## FBpp0071825 FBpp0309036 FBpp0086841 FBpp0111805 FBpp0083502 FBpp0075854 
    ##    7.511968    8.773469    8.800665    9.535452    9.697410   10.698989 
    ## FBpp0110410 FBpp0072029 FBpp0077839 FBpp0086795 FBpp0073974 FBpp0311639 
    ##   10.031620    6.347601    8.674096    9.033623   10.445015   10.706276 
    ## FBpp0070306 FBpp0292879 FBpp0078416 FBpp0312189 FBpp0074513 FBpp0306392 
    ##    8.485061    6.861595   14.017482    9.733693   10.288845    8.992398 
    ## FBpp0083972 FBpp0292258 FBpp0079892 FBpp0300658 FBpp0086334 FBpp0085560 
    ##    8.575917    7.676149   10.754872    7.565293    5.036423    6.845262 
    ## FBpp0290229 FBpp0072687 FBpp0073805 FBpp0076655 FBpp0308558 FBpp0083411 
    ##    6.595851   14.637806    8.341387    3.925999    9.727642    8.917182 
    ## FBpp0071703 FBpp0071794 FBpp0082953 FBpp0076134 FBpp0300391 FBpp0082877 
    ##   11.140748   13.747981    8.433339   11.768478    9.974362   10.782105 
    ## FBpp0073058 FBpp0079550 FBpp0306730 FBpp0309239 FBpp0086067 FBpp0075676 
    ##    8.064130   10.245834    6.951868    7.300290   10.884032    8.819831 
    ## FBpp0290083 FBpp0310192 FBpp0081744 FBpp0076643 FBpp0091107 FBpp0086666 
    ##   11.507732   10.228841    8.549627   11.676518    5.901196    6.690061 
    ## FBpp0306203 FBpp0311405 FBpp0292380 FBpp0308546 FBpp0311452 FBpp0088502 
    ##    8.305191    8.998924    7.495201    7.578802   14.242557    6.909075 
    ## FBpp0305517 FBpp0071476 FBpp0076142 FBpp0304016 FBpp0110438 FBpp0079352 
    ##    6.892391   10.015136    9.768365    8.329613    9.276378    7.932134 
    ## FBpp0073459 FBpp0310669 FBpp0308423 FBpp0309555 FBpp0291626 FBpp0071587 
    ##    8.790748    3.365845    4.485427    8.675251    8.570615   11.041385 
    ## FBpp0085636 FBpp0110260 FBpp0100141 FBpp0303962 FBpp0083028 FBpp0072458 
    ##   10.078102    9.738799   10.983888    9.861679    7.421529    7.268829 
    ## FBpp0082758 FBpp0312460 FBpp0297522 FBpp0070058 FBpp0086844 FBpp0304101 
    ##    9.536230    8.532515   14.095273   13.386654   10.993723   11.754774 
    ## FBpp0081205 FBpp0081302 FBpp0311550 FBpp0300893 FBpp0082462 FBpp0088522 
    ##    9.782581    9.650889   10.656929   10.291234    9.267127   10.756208 
    ## FBpp0075043 FBpp0070924 FBpp0112365 FBpp0077004 FBpp0070244 FBpp0312506 
    ##   10.047074    8.123745    6.646215    9.285024    6.893495    8.919405 
    ## FBpp0312112 FBpp0311959 FBpp0087232 FBpp0305840 FBpp0081068 FBpp0311991 
    ##    6.430529   10.652845    9.173927    9.908611   10.686278    9.748254 
    ## FBpp0311484 FBpp0290699 FBpp0073983 FBpp0070102 FBpp0082549 FBpp0289083 
    ##    6.756101    3.542821   10.296770    8.729775    8.899659    9.053309 
    ## FBpp0308369 FBpp0077763 FBpp0289361 FBpp0082328 FBpp0312508 FBpp0309829 
    ##    9.178823    9.174880    6.867223    5.692346    8.220928    9.327328 
    ## FBpp0077149 FBpp0099895 FBpp0085725 FBpp0301972 FBpp0084017 FBpp0072096 
    ##    8.965889    9.614543   10.045859    5.758284    8.951073    9.317207 
    ## FBpp0087511 FBpp0076458 FBpp0311371 FBpp0305852 FBpp0079577 FBpp0080024 
    ##   10.049997    7.684717   10.644659   11.752312    8.555844    8.467309 
    ## FBpp0072021 FBpp0099646 FBpp0310878 FBpp0297621 FBpp0304354 FBpp0304385 
    ##    9.001638    8.914467    8.350738   10.483779    8.921192    8.656197 
    ## FBpp0297132 FBpp0082231 FBpp0309066 FBpp0289888 FBpp0081442 FBpp0081444 
    ##   13.269222    6.460028   10.026725    9.509379    6.656652    9.882838 
    ## FBpp0075261 FBpp0306219 FBpp0309195 FBpp0302818 FBpp0288705 FBpp0304290 
    ##    7.238926    9.975223    9.632698    8.732183    8.552983    9.605715 
    ## FBpp0081533 FBpp0312451 FBpp0088368 FBpp0303612 FBpp0088872 FBpp0312215 
    ##    8.205460    9.374723   11.518287    4.208744    5.597477    5.626439 
    ## FBpp0311394 FBpp0084626 FBpp0071535 FBpp0290642 FBpp0086751 FBpp0075284 
    ##   12.548664    9.296624    7.269339    9.036773    7.096308    9.281277 
    ## FBpp0310472 FBpp0086582 FBpp0083549 FBpp0305746 FBpp0073104 FBpp0078655 
    ##    8.670495    9.230828    3.812965    7.443546    8.270784   12.983875 
    ## FBpp0303791 FBpp0309221 FBpp0099899 FBpp0086024 FBpp0292215 FBpp0080390 
    ##    7.197708    7.416011   13.701911    9.109712   11.436967    8.812907 
    ## FBpp0304878 FBpp0304388 FBpp0071818 FBpp0112504 FBpp0309678 FBpp0311114 
    ##    9.308422   11.592491    8.574817   10.168285    9.788199    7.388565 
    ## FBpp0312005 FBpp0311872 FBpp0289675 FBpp0078449 FBpp0070930 FBpp0077885 
    ##    8.780286    9.092757    9.007919   10.976004    5.043599    8.514051 
    ## FBpp0077676 FBpp0079575 FBpp0074843 FBpp0305515 FBpp0311613 FBpp0084117 
    ##    7.821571    8.169834   11.018036   11.173416   14.833669    8.290689 
    ## FBpp0081123 FBpp0311123 FBpp0082645 FBpp0312034 FBpp0082770 FBpp0072184 
    ##    7.480588    3.671911    9.693107    9.935299    8.066774    8.856963 
    ## FBpp0085097 FBpp0087052 FBpp0288671 FBpp0078422 FBpp0079060 FBpp0293494 
    ##    5.691839    7.929339    9.167008    9.093430    7.122899    4.732368 
    ## FBpp0304588 FBpp0312566 FBpp0306658 FBpp0099812 FBpp0072187 FBpp0307641 
    ##    7.283888    7.089968    5.160422   10.346272    8.903608   11.472922 
    ## FBpp0088678 FBpp0088679 FBpp0099722 FBpp0071813 FBpp0088602 FBpp0312318 
    ##   10.480496   10.010185    9.694120    9.007053    4.412166    4.856171 
    ## FBpp0307010 FBpp0083131 FBpp0304368 FBpp0303494 FBpp0305596 FBpp0309933 
    ##    9.549557    7.972275    9.944220   10.100208    4.499411   10.761355 
    ## FBpp0084714 FBpp0088656 FBpp0088085 FBpp0080722 FBpp0075139 FBpp0303088 
    ##    8.543735   10.826736    6.391693   10.121039    8.327001    5.506353 
    ## FBpp0076651 FBpp0099504 FBpp0305227 FBpp0078810 FBpp0077511 FBpp0075764 
    ##    5.412166    8.398938    8.207236    9.567161   10.287565   14.177606 
    ## FBpp0305677 FBpp0306622 FBpp0075684 FBpp0083861 FBpp0087985 FBpp0074949 
    ##   10.607061   10.326920    8.737048   11.725552    6.315026    9.819540 
    ## FBpp0075561 FBpp0311917 FBpp0084027 FBpp0086314 FBpp0303596 FBpp0071087 
    ##   10.549995   11.245175    6.999795    9.225755    5.988478    7.255672 
    ## FBpp0290569 FBpp0076792 FBpp0306232 FBpp0311276 FBpp0087259 FBpp0303108 
    ##   10.123792    8.553821   15.777008   12.186829    4.903389    9.747216 
    ## FBpp0306279 FBpp0292882 FBpp0304342 FBpp0293781 FBpp0073384 FBpp0303666 
    ##   11.616365   11.259133    7.362342    7.128346    3.355631   12.088517 
    ## FBpp0072477 FBpp0070979 FBpp0308011 FBpp0311226 FBpp0085489 FBpp0304563 
    ##   10.553018    4.447459    8.382447   10.837972   10.943714   11.532444 
    ## FBpp0075749 FBpp0311514 FBpp0079833 FBpp0070543 FBpp0303999 FBpp0112315 
    ##    6.395125   10.846203    8.595851    9.285360    8.021157    9.418658 
    ## FBpp0112316 FBpp0086393 FBpp0309967 FBpp0303780 FBpp0087004 FBpp0303294 
    ##    9.063346    8.384882    8.824583   13.524143    8.948049    9.792968 
    ## FBpp0082543 FBpp0080694 FBpp0292321 FBpp0310359 FBpp0076155 FBpp0082297 
    ##    8.470936    6.974154    8.510674    3.750022   10.365388    9.551096 
    ## FBpp0291576 FBpp0088620 FBpp0090982 FBpp0292059 FBpp0304400 FBpp0075715 
    ##    9.501946    8.659769    8.470788    6.208744   10.745369   10.025467 
    ## FBpp0304082 FBpp0070457 FBpp0301687 FBpp0309589 FBpp0308313 FBpp0305844 
    ##    9.152654   10.097841    6.969347    6.062169    6.706731    9.431707 
    ## FBpp0086266 FBpp0075431 FBpp0290046 FBpp0080721 FBpp0310397 FBpp0112197 
    ##   13.209221   10.535152   10.374486    7.092277    9.667918    8.258587 
    ## FBpp0077230 FBpp0304503 FBpp0074664 FBpp0311460 FBpp0112333 FBpp0078240 
    ##    6.508083    8.546123   10.887331   14.951063    9.114973   12.669988 
    ## FBpp0084247 FBpp0310721 FBpp0072460 FBpp0078383 FBpp0305707 FBpp0081882 
    ##    8.606456    8.668627    7.440831   10.396644    6.293451   11.345486 
    ## FBpp0074318 FBpp0086002 FBpp0081480 FBpp0081481 FBpp0088692 FBpp0081814 
    ##    9.368943   12.491019    8.667273    8.829896    9.812061    6.925136 
    ## FBpp0271799 FBpp0303146 FBpp0310411 FBpp0302674 FBpp0079258 FBpp0290319 
    ##    6.386999   10.697852   11.593934    9.008530    8.571649    4.614174 
    ## FBpp0072224 FBpp0077674 FBpp0077326 FBpp0083696 FBpp0081159 FBpp0303214 
    ##    8.417009    6.975822    8.607196    5.841607   10.300041   10.541554 
    ## FBpp0311414 FBpp0306948 FBpp0087086 FBpp0073989 FBpp0080687 FBpp0110412 
    ##    8.569510    8.536053   12.505148   10.856878   11.548051   13.828448 
    ## FBpp0074687 FBpp0081148 FBpp0293332 FBpp0073900 FBpp0304862 FBpp0081317 
    ##    8.361226    8.582364    4.739259    7.568751    5.110424   10.058533 
    ## FBpp0072881 FBpp0304165 FBpp0112438 FBpp0071255 FBpp0311603 FBpp0110179 
    ##    8.088620    9.914684   10.730331    6.806652   10.711119    8.270954 
    ## FBpp0290333 FBpp0070333 FBpp0311994 FBpp0297282 FBpp0081544 FBpp0085373 
    ##   10.905961    9.578253    9.994302   12.022538   11.677992   10.967711 
    ## FBpp0301800 FBpp0074161 FBpp0311178 FBpp0088190 FBpp0305835 FBpp0082998 
    ##    6.394813   11.085581   10.319487   10.713277   11.289149    8.452485 
    ## FBpp0083126 FBpp0070302 FBpp0083630 FBpp0310876 FBpp0074707 FBpp0076001 
    ##    9.078903    8.390755   10.206237    9.380205    9.137410    8.744528 
    ## FBpp0085195 FBpp0292371 FBpp0112608 FBpp0081754 FBpp0073173 FBpp0307198 
    ##   12.381735    9.111752    9.576605   10.343872   10.633111    8.723832 
    ## FBpp0087335 FBpp0083757 FBpp0087499 FBpp0070814 FBpp0306039 FBpp0311507 
    ##    6.697157    7.096883    9.459916    9.189780   14.543956   10.616148 
    ## FBpp0110166 FBpp0075136 FBpp0081910 FBpp0075148 FBpp0080255 FBpp0071631 
    ##   14.493072   15.042155    7.786002    8.923949    8.466642   10.754888 
    ## FBpp0070654 FBpp0307128 FBpp0077416 FBpp0079203 FBpp0088528 FBpp0289706 
    ##    5.552914    7.639127    8.291108    7.799488    7.234230    9.614141 
    ## FBpp0302795 FBpp0086535 FBpp0306251 FBpp0082803 FBpp0303867 FBpp0099725 
    ##    9.611058    9.665530    7.546193    8.758102    6.811097    9.743580 
    ## FBpp0099726 FBpp0075080 FBpp0070994 FBpp0074760 FBpp0288916 FBpp0288915 
    ##   12.771628    9.521600    9.882476    3.401035   11.178620   10.838989 
    ## FBpp0087146 FBpp0305186 FBpp0077109 FBpp0308232 FBpp0084930 FBpp0288730 
    ##   11.762556   10.054592    6.257902   11.385431    9.022216    8.267808 
    ## FBpp0080710 FBpp0082729 FBpp0082570 FBpp0079429 FBpp0302535 FBpp0302530 
    ##    8.028207    8.526268    8.571098   10.140272    6.219345    9.865115 
    ## FBpp0072802 FBpp0304595 FBpp0090944 FBpp0290912 FBpp0311627 FBpp0082849 
    ##   14.972505   10.038570    8.001434   10.042256    8.665659    7.985790 
    ## FBpp0309932 FBpp0079111 FBpp0082154 FBpp0309815 FBpp0086244 FBpp0086845 
    ##    8.475954   10.306041    6.001229   10.289935    9.814830    8.562242 
    ## FBpp0076861 FBpp0084471 FBpp0084120 FBpp0087676 FBpp0078602 FBpp0082737 
    ##    8.226894   15.020743   10.617100    9.290354   11.264379    9.541941 
    ## FBpp0306422 FBpp0311282 FBpp0302782 FBpp0305990 FBpp0083507 FBpp0072097 
    ##    9.966178   11.045983    5.657952   10.970394    9.086836   14.121300 
    ## FBpp0312110 FBpp0079472 FBpp0082850 FBpp0071350 FBpp0111884 FBpp0081800 
    ##   11.965325   11.280634    8.652421    7.796069    4.081278    9.273117 
    ## FBpp0312478 FBpp0305462 FBpp0293064 FBpp0311887 FBpp0074123 FBpp0081872 
    ##   11.443735   11.387283    9.368229   10.620120    9.666111    3.978528 
    ## FBpp0293004 FBpp0083072 FBpp0076608 FBpp0081834 FBpp0310043 FBpp0076868 
    ##    7.685864    7.254299   10.586806   11.210118    3.279336    8.896525 
    ## FBpp0291019 FBpp0071681 FBpp0311873 FBpp0081548 FBpp0072570 FBpp0072569 
    ##   11.107181    9.527548    7.311893    9.109190   10.581817   10.691918 
    ## FBpp0113013 FBpp0309017 FBpp0289214 FBpp0312030 FBpp0309363 FBpp0075445 
    ##    9.255243    9.894763    6.990542    8.925460    7.699809    7.037622 
    ## FBpp0307732 FBpp0304934 FBpp0071609 FBpp0077043 FBpp0081459 FBpp0309566 
    ##    9.872605    9.973006    9.590111    8.496218    9.312842   11.005089 
    ## FBpp0308311 FBpp0087182 FBpp0309344 FBpp0080872 FBpp0308273 FBpp0306890 
    ##    6.991367    8.721907    5.351140    9.967043    7.837829    9.218464 
    ## FBpp0302969 FBpp0070037 FBpp0308926 FBpp0309765 FBpp0079574 FBpp0311535 
    ##    5.910820   11.898436   10.029538    9.686532    7.549137    8.213966 
    ## FBpp0307576 FBpp0091111 FBpp0071600 FBpp0311533 FBpp0111906 FBpp0311555 
    ##    3.663655   12.458751    9.886417   10.751696    9.365328    8.657757 
    ## FBpp0071516 FBpp0309685 FBpp0110478 FBpp0088027 FBpp0079641 FBpp0086820 
    ##    9.825797    9.820759    8.944220    8.907874    9.476507    8.540568 
    ## FBpp0075400 FBpp0312108 FBpp0084329 FBpp0085562 FBpp0075508 FBpp0303919 
    ##    6.238231    7.720851    8.828281    8.755373   12.097110    9.136525 
    ## FBpp0087583 FBpp0311922 FBpp0300656 FBpp0083740 FBpp0072334 FBpp0307770 
    ##   12.746476    7.448360    8.334659    9.779690    6.792761   14.291037 
    ## FBpp0088505 FBpp0302735 FBpp0307181 FBpp0305564 FBpp0080121 FBpp0070935 
    ##   13.965542    8.360827   10.656864    4.745140    8.555356    7.546614 
    ## FBpp0307367 FBpp0082127 FBpp0303516 FBpp0303319 FBpp0100089 FBpp0077357 
    ##    8.094869    7.424892   10.139016    5.755373    6.401655    7.063346 
    ## FBpp0077129 FBpp0305743 FBpp0073088 FBpp0085119 FBpp0073098 FBpp0087607 
    ##   10.513207    7.099563    9.844834    8.863679    9.414704    9.554135 
    ## FBpp0081504 FBpp0073791 FBpp0099814 FBpp0073421 FBpp0305845 FBpp0081552 
    ##    8.679736    9.965050    6.334659    6.958843    6.064130   10.517884 
    ## FBpp0311716 FBpp0302766 FBpp0087437 FBpp0083546 FBpp0306398 FBpp0293275 
    ##    4.409700   10.607213   10.405548    6.386058    8.567922   10.043549 
    ## FBpp0311274 FBpp0310687 FBpp0080320 FBpp0293601 FBpp0309831 FBpp0075534 
    ##    8.211313   11.270709   10.365865    6.627501    5.312058    7.105289 
    ## FBpp0309283 FBpp0307590 FBpp0074660 FBpp0074661 FBpp0074729 FBpp0070864 
    ##   10.527761    7.334497    9.148176    7.770949    9.685641    8.694184 
    ## FBpp0082571 FBpp0084778 FBpp0087973 FBpp0087206 FBpp0075697 FBpp0083160 
    ##   12.251450    8.753065    8.689299    6.214407    9.651476    9.175788 
    ## FBpp0083923 FBpp0307666 FBpp0079267 FBpp0112047 FBpp0309820 FBpp0297362 
    ##    9.351662   11.604477    8.065012    9.099659    8.502452   11.220730 
    ## FBpp0073149 FBpp0306543 FBpp0084012 FBpp0073196 FBpp0077011 FBpp0311629 
    ##    9.382722    5.034824    8.306932    8.243607    8.957893    7.409854 
    ## FBpp0081451 FBpp0081582 FBpp0086271 FBpp0073148 FBpp0086702 FBpp0309238 
    ##    7.793234    8.999846    4.819482    9.864608    7.603019   10.025014 
    ## FBpp0292109 FBpp0075864 FBpp0075938 FBpp0081187 FBpp0303075 FBpp0311631 
    ##    6.078175    4.613103   10.036873    7.399793    6.745873    4.648047 
    ## FBpp0309034 FBpp0292351 FBpp0084155 FBpp0113041 FBpp0310080 FBpp0085481 
    ##   10.306683    8.311480    7.624579   12.164907    9.133726    6.665982 
    ## FBpp0304799 FBpp0082242 FBpp0072848 FBpp0291346 FBpp0087939 FBpp0088018 
    ##    8.968248    6.433263   10.970132   10.399754   11.567273   10.563526 
    ## FBpp0083610 FBpp0074525 FBpp0305702 FBpp0297081 FBpp0073982 FBpp0303585 
    ##    6.009396   10.139133   13.306284   13.459681    8.986566    9.078612 
    ## FBpp0087055 FBpp0290487 FBpp0072112 FBpp0288766 FBpp0309306 FBpp0293874 
    ##    7.351622    7.117808   10.427027   11.216911    9.643430    8.474407 
    ## FBpp0073828 FBpp0271901 FBpp0289952 FBpp0083665 FBpp0080855 FBpp0079584 
    ##   10.462000    9.424624    9.242395    8.666434    7.444149    8.022267 
    ## FBpp0078388 FBpp0306644 FBpp0297504 FBpp0072641 FBpp0083238 FBpp0073120 
    ##    8.916911   11.033986    7.209453    7.573441   10.202035    7.924920 
    ## FBpp0311482 FBpp0082326 FBpp0309739 FBpp0078431 FBpp0084545 FBpp0311161 
    ##    9.607431    7.307429   11.608944   10.682484    9.874562   11.734602 
    ## FBpp0079801 FBpp0305994 FBpp0301711 FBpp0307618 FBpp0304260 FBpp0082510 
    ##    7.978320    7.467902    7.042406   10.954500   17.722109    9.802516 
    ## FBpp0311982 FBpp0293863 FBpp0303400 FBpp0079576 FBpp0305024 FBpp0288974 
    ##   12.183444    8.223387    9.649844    9.763756    9.663946   12.899134 
    ## FBpp0309996 FBpp0312428 FBpp0084196 FBpp0079616 FBpp0311678 FBpp0086115 
    ##   12.441742   12.494237    6.289767    9.804160    5.732368    9.301496 
    ## FBpp0072060 FBpp0309617 FBpp0083005 FBpp0308258 FBpp0301991 FBpp0081593 
    ##   11.498831    7.411704    8.014578    8.694374    9.505306    8.630482 
    ## FBpp0086597 FBpp0309051 FBpp0289447 FBpp0296928 FBpp0082392 FBpp0086207 
    ##    7.744283    7.571511    6.549277    9.369815    8.986980   10.863426 
    ## FBpp0076789 FBpp0078124 FBpp0310069 FBpp0290546 FBpp0310068 FBpp0081288 
    ##    6.543665    8.202792   10.513117   10.487895    3.420764    7.723087 
    ## FBpp0308778 FBpp0073440 FBpp0078054 FBpp0072145 FBpp0082973 FBpp0289444 
    ##    8.545702    8.283888    9.031169    9.574748    9.503609    9.649191 
    ## FBpp0293211 FBpp0099977 FBpp0291491 FBpp0089109 FBpp0311799 FBpp0312573 
    ##    9.267893    8.720291   10.873388   11.324845   13.817531    9.628495 
    ## FBpp0088139 FBpp0304824 FBpp0289452 FBpp0086323 FBpp0111303 FBpp0087479 
    ##    9.051431   10.002380    6.357872    8.213347    8.663332   14.032412 
    ## FBpp0072720 FBpp0073017 FBpp0291732 FBpp0288739 FBpp0087636 FBpp0307408 
    ##    9.189196   10.256595    8.712558    6.139737    9.988891    7.853792 
    ## FBpp0297354 FBpp0083969 FBpp0311543 FBpp0086984 FBpp0081401 FBpp0083134 
    ##    7.747583    7.464193    9.080939    7.859111   12.728222    8.929608 
    ## FBpp0083769 FBpp0302571 FBpp0076705 FBpp0076656 FBpp0088287 FBpp0310817 
    ##    8.173836    9.710337    4.067263    6.225668   11.699667    8.371876 
    ## FBpp0303487 FBpp0074381 FBpp0309384 FBpp0304108 FBpp0288791 FBpp0288779 
    ##    8.567783   10.502868   11.904662   10.195495    6.642807   11.422629 
    ## FBpp0291674 FBpp0309447 FBpp0087062 FBpp0099892 FBpp0306952 FBpp0080778 
    ##    5.861143    8.512040   10.350819   11.874974    5.224966    7.763605 
    ## FBpp0304364 FBpp0111294 FBpp0075930 FBpp0080264 FBpp0081704 FBpp0309074 
    ##    4.911256    9.366441   10.180384    5.420764   10.092973    9.397889 
    ## FBpp0086261 FBpp0088881 FBpp0300167 FBpp0300169 FBpp0070723 FBpp0083818 
    ##   10.260855   12.377718   11.332433   11.374160    8.722963    8.993376 
    ## FBpp0082111 FBpp0079171 FBpp0308244 FBpp0311888 FBpp0304265 FBpp0305258 
    ##   10.791548   11.562208    5.481322    9.275490   13.908534    8.878190 
    ## FBpp0298346 FBpp0077081 FBpp0077537 FBpp0086741 FBpp0290377 FBpp0290380 
    ##    8.587215   10.877912    5.987652    8.376303   10.020323    9.720477 
    ## FBpp0075250 FBpp0085264 FBpp0309483 FBpp0074213 FBpp0085466 FBpp0099923 
    ##    8.733231    9.401035    9.318029    9.914113    7.684462   11.802847 
    ## FBpp0085204 FBpp0070748 FBpp0081096 FBpp0074261 FBpp0305700 FBpp0311816 
    ##    9.679928    8.361625    9.262437    9.373537    9.034024   12.304937 
    ## FBpp0085255 FBpp0112117 FBpp0086965 FBpp0291553 FBpp0309175 FBpp0083076 
    ##   10.318563   11.486881    7.876963    7.679864    8.993221    9.386058 
    ## FBpp0072116 FBpp0075581 FBpp0307389 FBpp0088990 FBpp0304361 FBpp0073235 
    ##    8.471749   11.782805    7.853678   11.635644    8.036423    8.349211 
    ## FBpp0084466 FBpp0077277 FBpp0312199 FBpp0086767 FBpp0298306 FBpp0088517 
    ##    9.217936    9.973345   11.271208    8.512974    5.465381   11.668651 
    ## FBpp0289815 FBpp0311531 FBpp0076833 FBpp0311387 FBpp0293147 FBpp0293149 
    ##   14.044754   10.180994   11.910295   12.448083    4.517848    7.765655 
    ## FBpp0304061 FBpp0075395 FBpp0075104 FBpp0309390 FBpp0086674 FBpp0072035 
    ##    8.596664    8.960002   11.635009    5.383547    9.160605   12.686707 
    ## FBpp0082655 FBpp0309448 FBpp0087524 FBpp0304214 FBpp0070262 FBpp0297643 
    ##   10.154312   12.055184    8.929178    9.930872   13.854341   10.882421 
    ## FBpp0291922 FBpp0291923 FBpp0291924 FBpp0305374 FBpp0305376 FBpp0086994 
    ##   10.800076   10.657692   10.102644    8.522635    7.550817   10.015922 
    ## FBpp0087716 FBpp0308386 FBpp0271912 FBpp0074146 FBpp0110565 FBpp0293109 
    ##   10.875791   10.771189    9.928720    6.632797    9.327287    6.596664 
    ## FBpp0304253 FBpp0306915 FBpp0112205 FBpp0075119 FBpp0079091 FBpp0100186 
    ##   10.322727   12.014452    7.003480   10.700314    9.972014   15.709840 
    ## FBpp0072004 FBpp0311825 FBpp0073572 FBpp0304749 FBpp0298366 FBpp0076359 
    ##    8.222246   13.317798    9.816984    9.732059    9.535912   14.269097 
    ## FBpp0075013 FBpp0075012 FBpp0293864 FBpp0310529 FBpp0311454 FBpp0110337 
    ##    7.355471   11.234251    9.321436    7.782909   12.930129   10.400918 
    ## FBpp0290817 FBpp0085317 FBpp0308451 FBpp0303631 FBpp0079073 FBpp0071427 
    ##   10.270932    8.294955   10.466864   10.815834   10.068119   13.181172 
    ## FBpp0288543 FBpp0304443 FBpp0075260 FBpp0071178 FBpp0087399 FBpp0290496 
    ##    8.044096    8.988220    9.170335    9.360229    5.868795    7.296958 
    ## FBpp0305501 FBpp0082883 FBpp0308306 FBpp0072834 FBpp0072833 FBpp0312410 
    ##   10.916015   11.093141    9.271251    5.171382    6.747339    7.774787 
    ## FBpp0310321 FBpp0070129 FBpp0099820 FBpp0085775 FBpp0309737 FBpp0307647 
    ##    9.832171    7.685609    8.593069   10.347742    9.685832   10.750540 
    ## FBpp0088269 FBpp0312218 FBpp0311936 FBpp0303776 FBpp0077652 FBpp0070760 
    ##   10.262671    8.284561    8.549627    8.469235   10.794770   11.840942 
    ## FBpp0304264 FBpp0308772 FBpp0291114 FBpp0291113 FBpp0291632 FBpp0075718 
    ##    7.337906   10.505360    8.219873    8.257388   15.267095    9.245034 
    ## FBpp0081545 FBpp0071461 FBpp0071459 FBpp0292237 FBpp0073029 FBpp0303833 
    ##   10.976642   12.334385    9.654473    6.788021    9.528401    8.742937

1.  Calculate the mean expression for the subset of columns for large
    and small male head horns

<!-- -->

    #create subset of large male head horns
    lg_male_hdhorn <- subset(rna_counts, select=c( "M125_lg_male_hdhorn", "M160_lg_male_hdhorn", "M180_lg_male_hdhorn","M257_lg_male_hdhorn"))

    #create subset of small head horns 
    sm_male_hdhorn <- subset(rna_counts, select=c("M120_sm_male_hdhorn",  "M171_sm_male_hdhorn", "M172_sm_male_hdhorn", "M200_sm_male_hdhorn"))

    #create row names 
    rownames(sm_male_hdhorn) <- rna_counts[,1]
    rownames(lg_male_hdhorn) <- rna_counts[,1]

    #gene counts
    genecounts_sm_male_hdhorn <- apply(sm_male_hdhorn, 1, mean3, log2scale = FALSE)
    genecounts_lg_male_hdhorn <- apply(lg_male_hdhorn, 1, mean3, log2scale = FALSE)

    #gene counts log2 
    genecounts_sm_male_hdhorn_log2 <- apply(sm_male_hdhorn, 1, mean3)
    genecounts_lg_male_hdhorn_log2 <- apply(lg_male_hdhorn, 1, mean3)

    #calculate difference 
    difference <-(genecounts_lg_male_hdhorn - genecounts_sm_male_hdhorn)
    differencelog2 <- (genecounts_lg_male_hdhorn_log2 - genecounts_sm_male_hdhorn_log2)

1.  plot the mean expression of each gene and the difference in
    expression values

<!-- -->

    #calculate avg gene expression
    avgexp <- (genecounts_lg_male_hdhorn + genecounts_sm_male_hdhorn) / 2 
    avgexplog2 <- (genecounts_lg_male_hdhorn_log2 + genecounts_sm_male_hdhorn_log2) / 2 


    plot(avgexp, difference, main = "Average Gene Expression vs Difference", xlab = "Gene Expression", ylab = "Genes Expression Difference")

![](DH_Bio720_Assignment3_GC_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    plot(avgexplog2, differencelog2, main = "Log2 Average Gene Expression vs Difference", xlab = "Avg Gene Expression", ylab = "Gene Expression Difference")

![](DH_Bio720_Assignment3_GC_files/figure-markdown_strict/unnamed-chunk-8-2.png)

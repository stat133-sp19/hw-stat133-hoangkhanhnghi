Workout01
================
Nghi Nguyen

The purpose of this artical is to introduce the five most famous professional basketball players of NBA Golden State Warriors by analyzing their statistics in the 2016 season. They were Andre Iguodala, Draymond Green, Kevin Durant, Klay Thompson, and Stephen Curry. Some briefly information about those players is given below.

1/ Andre Tyler Iguodala <img src="andre-iguodala.jpg" width="80%" style="display: block; margin: auto;" /> Iguodala used to play for Philadelphia 76ers and Denver Nuggests before he came to the Warriors in 2013. His position in the Warriors is SF, and he helped the Warriors win NBA championship in 2015, 2017, and 2018.

2/ Draymond Green <img src="draymond-green.png" width="80%" style="display: block; margin: auto;" /> Green has never played for any NBA team before the Warriors. He plays primarily at the power forward position and also helps the Warriors to win NBA championship three times. In 2017, Green won the NBA Defensive Player of the Year.

3/ Kevin Durant <img src="kevin-durant.jpg" width="80%" style="display: block; margin: auto;" /> Durant is one of the highest-earn basketball players in the world. He has been in the Warriors from 2016 as a power forward. Before that, he played for Seattle SuperSonics in 2007-2008 season and Oklahoma City Thunder from 2008 to 2016.

4/ Klay Thompson <img src="thompson-klay.jpg" width="80%" style="display: block; margin: auto;" /> Thompson has played for the Warriors for nearly 9 years as a shooting guard. He is the son of former NBA player Mychal Thompson. In 2014, Thompson and teammate Stephen Curry set a NBA record with 484 combined 3-points in a season, making the pair nickname the "Splash Brothers"

5/ Stephen Curry <img src="stephen-curry.jpg" width="80%" style="display: block; margin: auto;" /> Curry has been named the NBA Most Valuable Player twice and also won the NBA championship with the Warriors three times. He is a point guard, and many professional players have called him the greatest shooter in NBA history.

In the 2016 regular season, five professional basketball players of the Golden State Warriors had a good performance. They played a total of 4335 games from the end of 2016 to the beginning of 2017. The below charts illustrate of every shot each player attempted during the 2016 season in Golden State Warriors. <img src="gsw-shot-chart.png" width="80%" style="display: block; margin: auto;" /> The picture doesn't show many of Iguodala attempts in getting three-point and two-point shots, but we could see that the ratio of made shots was quite high. In the meantime, Green seemed to focus more on three-point areas, and he also had a little more attempts than Iguodala did. One the other hand, Durant, Thompson, and Curry earned shot in all areas. The charts show that the three later players had many more attempts than Iguodala and Green but also had more than 50 percent of missed shots. One impressive point is that Iguodala, Curry, and Curry obtained some points from the center of the game fields.

The tables below give more details for the performances of the players in the 2016 season. 1/ 2PT Effective Shooting Percentage

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- read.csv("../Desktop/workout01/data/shots-data.csv", stringsAsFactors = FALSE)
shot <- data[data$shot_made_flag == "shot_yes",]

fga <- length(data$shot_type)

data1 <- data[data$shot_type == "2PT Field Goal",]
fgm1 <- length(data1$shot_type)
shot1 <- data1[data1$shot_made_flag == "shot_yes",]

new_data1 <- summarise(
  group_by(data1,name),
  total=n(),
)
newdata1 <- summarise(
  group_by(shot1,name),
  made=n(),
)
point_2 <- cbind(new_data1,made = newdata1$made)
point2 <- mutate(point_2, perc_made=made/total)
point2 <- data.frame(point2)
point2 <- arrange(point2,desc(perc_made))

point2
```

    ##             name total made perc_made
    ## 1 Andre Iguodala   210  134 0.6380952
    ## 2   Kevin Durant   643  390 0.6065319
    ## 3  Stephen Curry   563  304 0.5399645
    ## 4  Klay Thompson   640  329 0.5140625
    ## 5 Draymond Green   346  171 0.4942197

Andre Iguodala was named the NBA Finals Most Valuable Player in 2015 with the Warriors, and the 2-point effective shooting table shows that he gained the highest percentage of made shots, which is nearly 63.8 percent. This figure was impressive since it was far away from the lowest percentage (Green with 50 percent). Kevin Durant also had good performances with his two-point shots (60.7 percent). In general, all players successfully took the advantages of two-point areas.

2/ 3PT Effective Shooting Percentage

``` r
data2 <- data[data$shot_type == "3PT Field Goal",]
fgm2 <- length(data2$shot_type)
shot2 <- data2[data2$shot_made_flag == "shot_yes",]
new_data2 <- summarise(
  group_by(data2,name),
  total=n(),
)
newdata2 <- summarise(
  group_by(shot2,name),
  made=n(),
)
point_3 <- cbind(new_data2,made = newdata2$made)
point3 <- mutate(point_3, perc_made=made/total)
point3 <- data.frame(point3)
point3 <- arrange(point3,desc(perc_made))

point3
```

    ##             name total made perc_made
    ## 1  Klay Thompson   580  246 0.4241379
    ## 2  Stephen Curry   687  280 0.4075691
    ## 3   Kevin Durant   272  105 0.3860294
    ## 4 Andre Iguodala   161   58 0.3602484
    ## 5 Draymond Green   232   74 0.3189655

The 3-point shot is one of the most important scoring options on the court, and it might not easy even for contemporary players. Comparing to the 2-point effective shooting table, the statistics is less positive in the three-point effective shooting table. All players got effective rates of lower than 50 percent. For example, Thomspon was the most effective 3-point shooting player, but his data was only 42.2 percent. Similarly, Iguodala didn't do really well with around 36 made shots out of 100. Green still had the lowest effective shots when gaining only 31.9 percent of made shots in three-point areas.

3/ Effective Shooting Percentage

``` r
new_data <- summarise(
  group_by(data,name),
  total=n(),
)
newdata <- summarise(
  group_by(shot,name),
  made=n(),
)
point_total <- cbind(new_data,made = newdata$made)
point <- mutate(point_total, perc_made=made/total)
point <- data.frame(point)
point <- arrange(point,desc(perc_made))
point
```

    ##             name total made perc_made
    ## 1   Kevin Durant   915  495 0.5409836
    ## 2 Andre Iguodala   371  192 0.5175202
    ## 3  Klay Thompson  1220  575 0.4713115
    ## 4  Stephen Curry  1250  584 0.4672000
    ## 5 Draymond Green   578  245 0.4238754

In overall, Durant obtained a higher total effective shooting percentage than other players (about 54.1 percent). Although Curry's total shot and total made-shot were significant (1250 attempts and 584 made shots), his effective shooting percentage was not that high. Thompson was in the same situation when he made 1220 attempts and achieved 584 made shots. However, the effective percentage was not everything as getting about 570 made shots in a season could bring several victories to the Warriors. Said, Curry and Thompson were still amazing shooters.

``` r
mean(point$perc_made)
```

    ## [1] 0.4841781

The average effective shooting percentage of the five players went lower than 50 percent in the 2016 season.

The following figure illustrates the average point that each player made on an attempt. The ranking is not very different from the total effective shooting percentage table. Durant, who had the highest total effective shooting percentage, earned the highest point per one attempt (1.197). In the meantime, Iguodala got nearly the same point (1.191), Curry got 1.158, Thompson got 1.144, and the data of Green was 0.976.

``` r
df1 <- mutate(new_data,point_per_attempt = ((2*newdata1$made + 3*newdata2$made)/new_data$total))
arrange(df1,desc(point_per_attempt))
```

    ## # A tibble: 5 x 3
    ##   name           total point_per_attempt
    ##   <chr>          <int>             <dbl>
    ## 1 Kevin Durant     915             1.20 
    ## 2 Andre Iguodala   371             1.19 
    ## 3 Stephen Curry   1250             1.16 
    ## 4 Klay Thompson   1220             1.14 
    ## 5 Draymond Green   578             0.976

The next data comes from the Basketball Reference. It includes many general statistics of Golden State Warriors in the 2016-2017 season. Let's focus on the salaries table first. <img src="Salary.png" width="80%" style="display: block; margin: auto;" /> Durant, Thompson, Green, Curry, and Iguodala were the top five highest-paid players at the time. Durant made the most amount of money in the Warriors team due to his amazing performances; he received 26,540,100 dollars after the season. Thompson was after Durant with 16,663,575 dollars. There was about 10,000,000 dollars difference in the salary of Durant and Thompson. It was surprised that Green was in the third place when making 15,330,435 dollars in this season while Curry earned only 12,112,359 dollars. Lastly, even though Iguodala had an outstanding effective rate and point per attempt, he got only half of Durant's salary, and it is also the lowest salary among five players (11,131,368 dollars). The low number of attempt might be the reason for Iguodala's low salary level.

The article has given a general analysis of five professional basketball players of NBA Golden States Warriors in the season 2016-2017. Accordingly, we can see some references between their performance and their salary. Kevin Durant seems to be the most outstanding player due to his tremendous salary and effectiveness.

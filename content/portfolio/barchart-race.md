---
date: "2016-11-05T19:41:01+05:30"
draft: false
image: img/portfolio/barplot_static.jpg
showonlyimage: false
title: Barchart Races With gganimate
weight: 1
---

![Alt Text](https://raw.githubusercontent.com/emily-kuehler/personal-site/master/public/img/barplot_race.gif)

# Grand Slam Tennis Bar Chart Race

Awhile back, I put together a tutorial for a barchart race using gganimate. Folks still seem to visit the post so I wanted to clean that up a bit as I'm putting together my new site. The original was inspired by a Tidy Tuesday exercise and some work John Burn-Murdoch had done with bar chart races in D3. This will follow the original extremely closely, but hopefully look a little better for the new site.

## Data Wrangling

First, we’ll load the necessary packages and read in our data.

```
#get packages
library(tidyverse); library(gganimate)

#load data
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
```

So we have a tibble with six variables:

+ `year`: year in which the grand slam was played 
+ `grand_slam`: the name of the tournament 
+ `name`: champion of corresponding tournament 
+ `rolling_win_count`: rolling count of grand slam wins for corresponding player 
+ `tournament_date`: approximate tournament date 
+ `gender`: player gender

In order to prep the data for our bar chart race in gganimate, we need to group the data by each point in time and find players with the top 10 rolling win count for each point in time. We define a point in time as the completion of a grand slam tournament. Also, for this graphic, we will construct a bar chart of the top 10 wins from all players, rather than separating by gender.

For the first few years in the dataset, the scant data at this point in time results in either a very sparse bar chart or a bar chart filled with ties at the bottom. Because of this, I chose to start the bar chart race in 1975, when there was enough data to create a full top 10 bar chart without a ton of ties at the bottom, which would distort the shape of the graph.

The first step we’ll take is to give a numeric ordering to the tournaments and arrange the data by tournament date.

```
grand_slams_clean <- grand_slams %>% 
  mutate(tournament_order = case_when(grand_slam=='australian_open' ~ 1,
                                      grand_slam=='french_open' ~ 2,
                                      grand_slam=='wimbledon' ~ 3,
                                      grand_slam=='us_open' ~ 4)) %>%
  arrange(tournament_date)
```

Next, we’ll go through a two step process to prepare our data for gganimate. As noted before, we are going to start in 1975 because of the multiple ties that existed in previous years. The grouping for 1975 will consist of the players with the top 10 rolling wins from 1968 through 1975 (i.e. all the tournaments of 1975). We will label this `init_df`. After this each grouping will be a point in time, which is after a single grand slam tournament.

```
#get data from 1968-1975, helps avoid ties or incomplete bar chart at beginning
init_df <- grand_slams_clean %>%
  filter(year <= 1975) %>%
  group_by(name) %>%
  filter(rolling_win_count==max(rolling_win_count)) %>%
  ungroup() %>%
  top_n(10, wt=rolling_win_count) %>%
  arrange(desc(rolling_win_count)) %>%
  select(name,gender, rolling_win_count) %>%
  mutate(curr_year = 1975,
         ordering = as.double(rev(seq(10:1))) * 1.0)

#outer loop gets year
for (i in 1976:2019) {
  #inner loop gets tournament
  for (j in 1:4) {
    tmp_df <- grand_slams_clean %>%
      #filter data up to correct point in time
      filter(year < i | (year==i & tournament_order <= j)) %>%
      #get each players max win count
      group_by(name) %>% 
      filter(rolling_win_count==max(rolling_win_count)) %>% 
      ungroup() %>% 
      top_n(10, wt=rolling_win_count) %>%
      select(name, gender, rolling_win_count) %>%
      arrange(desc(rolling_win_count)) %>%
      slice(1:10) %>%
      #add var for curr_year, ordering for easy bar chart (reverse it cuz we're make a horizontal chart)
      mutate(curr_year = i,
             tournament_num = j,
             ordering = as.double(rev(seq(10:1))) * 1.0) 
    init_df <- init_df %>%
      bind_rows(tmp_df)
  }
}
```
So this is starting to look pretty good. However, when we put the data into gganimate, we want each frame to transition after a grand slam tournament. Right now, we have the variables curr_year, however there are four tournaments in a year, so that transition is too long, and tournament_num, which identifies tournaments correctly, but not uniquely. But, we can use these variables to create a unique id and easily plug that into gganimate.

```
final_df <- init_df %>% 
  group_by(curr_year, tournament_num) %>% 
  mutate(frame_id = group_indices()) %>% 
  ungroup()
```

Before diving into our plot, let’s take a look at player names, which we’re going to be using as labels on the bars.

```
unique(final_df$name)
```

Here we see `Evonne Goolagong Cawley` is VERY long. During much of her playing career she went by `Evonne Goolagong` so I don’t feel terrible about dropping her married name for labeling ease. `Martina Navratilova` is also quite long, but that’s just gonna stay because there is no great way to shorten it.

```
final_df <- final_df %>% mutate(name = ifelse(name == 'Evonne Goolagong Cawley', 'Evonne Goolagong', name))
```

## Constructing the Plot -- The Fun Part!

Ok, now that we have our data in the right format, we can go ahead and make our plot. The first thing we’ll do is set our theme, palette and font.

Quick note on the font as I got a couple questions -- I used a non-standard font downloaded from Google Fonts for this plot. It is fairly straightforward to get this working, but out of scope of this tutorial. You can always subsitute the font of choice that you know is installed on your computer.

```
my_font <- 'Quicksand Light'
my_background <- 'antiquewhite'
my_pal <- c('#F8AFA8','#74A089') #colors for bars (from wesanderson)
my_theme <- my_theme <- theme(text = element_text(family = my_font),
                              rect = element_rect(fill = my_background),
                              plot.background = element_rect(fill = my_background, color = NA),
                              panel.background = element_rect(fill = my_background, color = NA),
                              panel.border = element_blank(),
                              plot.title = element_text(face = 'bold', size = 20),
                              plot.subtitle = element_text(size = 14),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_line(color = 'grey75'),
                              panel.grid.minor.x = element_line(color = 'grey75'),
                              legend.position = 'none',
                              plot.caption = element_text(size = 8),
                              axis.ticks = element_blank(),
                              axis.text.y =  element_blank())

theme_set(theme_light() + my_theme)
```

### Make the Plot

So the question for the bar chart race in gganimate was how to make the transitions between time periods smooth. Thankfully, I found [this EXTREMELY helpful stackoverflow post](https://stackoverflow.com/questions/52623722/how-does-gganimate-order-an-ordered-bar-time-series/52652394#52652394) that directed me toward the use of geom_tile() and saved a lot of time when I otherwise probably would have wasted a lot of time testing out geom_bar().


```
final_plot <- ggplot(aes(ordering, group = name), data = final_df) +
  geom_tile(aes(y = rolling_win_count / 2, 
                height = rolling_win_count,
                width = 0.9, fill=gender), alpha = 0.9) +
  scale_fill_manual(values = my_pal) +
  geom_text(aes(y = rolling_win_count, label = name), family=my_font, nudge_y = -2, size = 3) +
  #convert to character to get rid of blurry trailing decimals
  geom_text(aes(y = rolling_win_count, label = as.character(rolling_win_count)), family=my_font, nudge_y = 0.5) +
  geom_text(aes(x=1,y=18.75, label=paste0(curr_year)), family=my_font, size=8, color = 'gray45') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(title = 'Most Grand Slam Singles Championships',
       subtitle = 'Open Era Only',
       caption = 'data source: Wikipedia | plot by @emilykuehler',
       x = '',
       y = '') +
  transition_states(frame_id, 
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')
```

![Alt Text](https://raw.githubusercontent.com/emily-kuehler/personal-site/master/public/img/barplot_race.gif)

This was my first time working with gganimate and in addition to the posts already mentioned, I found [this post](https://github.com/ropenscilabs/learngganimate/blob/2872425f08392f9f647005eb19a9d4afacd1ab44/animate.md#saving-your-animation) by [Will Chase](https://twitter.com/w_r_chase?lang=en) to be invaluable. 














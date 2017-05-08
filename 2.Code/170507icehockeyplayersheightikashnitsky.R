################################################################################
#                                                    
# Hight of ice-hockey playes 2017-05-07
# This script reproduces all the analysis and dataviz for the blog post
# https://ikashnitsky.github.io/2017/ice-hockey-players-height/
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#                                                  
################################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))

# load required packages
library(tidyverse) # data manipulation and viz
library(lubridate) # easy manipulations with dates
library(ggthemes) # themes for ggplot2
library(texreg) # easy export of regression tables
library(xtable) # export a data frame into an html table
library(sysfonts) # change the font in figures


# download the IIHF data set; if there are some problems, you can download manually
# using the stable URL (https://dx.doi.org/10.6084/m9.figshare.3394735.v2)
df <- read.csv("https://ndownloader.figshare.com/files/5303173")

# color palette
brbg11 <- RColorBrewer::brewer.pal(11, "BrBG")


################################################################################
# rough comparison

df_per <- df %>% group_by(year) %>%
        summarise(height = mean(height))

gg_period_mean <- ggplot(df_per, aes(x = year, y = height))+
        geom_point(size = 3, color = brbg11[9])+
        stat_smooth(method = "lm", size = 1, color = brbg11[11])+
        ylab("height, cm")+
        xlab("year of competition")+
        scale_x_continuous(breaks = seq(2005, 2015, 5), labels = seq(2005, 2015, 5))+
        theme_few(base_size = 15, base_family = "mono")+
        theme(panel.grid = element_line(colour = "grey75", size = .25))


gg_period_jitter <- ggplot(df, aes(x = year, y = height))+
        geom_jitter(size = 2, color = brbg11[9], alpha = .25, width = .75)+
        stat_smooth(method = "lm", size = 1, se = F, color = brbg11[11])+
        ylab("height, cm")+
        xlab("year of competition")+
        scale_x_continuous(breaks = seq(2005, 2015, 5), labels = seq(2005, 2015, 5))+
        theme_few(base_size = 15, base_family = "mono")+
        theme(panel.grid = element_line(colour = "grey75", size = .25))

gg_period <- cowplot::plot_grid(gg_period_mean, gg_period_jitter)

ggsave("fig-01-period-height.png", gg_period, width = 11, height = 5)




################################################################################
# turn to cohort data 
# remove double counts

dfu_h <- df %>% select(year, name, country, position, birth, cohort, height) %>%
        spread(year, height)
dfu_h$av.height <- apply(dfu_h[, 6:21], 1, mean, na.rm = T)
dfu_h$times_participated <- apply(!is.na(dfu_h[, 6:21]), 1, sum)

dfu_w <- df %>% select(year, name, country, position, birth, cohort, weight) %>%
        spread(year, weight)
dfu_w$av.weight <- apply(dfu_w[, 6:21], 1, mean, na.rm = T)


dfu <- left_join(dfu_h %>% select(name, country, position, birth, cohort, av.height, times_participated), 
                 dfu_w %>% select(name, country, position, birth, cohort, av.weight), 
                 by = c("name", "country", "position", "birth", "cohort")) %>%
        mutate(bmi = av.weight / (av.height / 100) ^ 2)


################################################################################
# times participated

# frequencies of participation in world championships

mean(dfu$times_participated)

df_part <- as.data.frame(table(dfu$times_participated))

gg_times_part <- ggplot(df_part, aes(y = Freq, x = Var1))+
        geom_bar(stat = "identity", fill = brbg11[8])+
        ylab("# of players")+
        xlab("times participated (out of 16 possible)")+
        theme_few(base_size = 15, base_family = "mono")+
        theme(panel.grid = element_line(colour = "grey75", size = .25))


ggsave("fig-02-times-part.png", gg_times_part, width = 9, height = 5)


# the leaders of participation in world championships
leaders <- dfu %>% filter(times_participated > 9)
View(leaders)
# save the table to html
print(xtable(leaders), type = "html", file = "table_leaders.html")


# countries times participated
df_cnt_part <- df %>% select(year, country, no) %>%
        mutate(country = factor(paste(country))) %>%
        group_by(country, year) %>%
        summarise(value = sum(as.numeric(no))) %>%
        mutate(value = 1) %>%
        ungroup() %>%
        mutate(country = factor(country, levels = rev(levels(country))), 
               year = factor(year))

d_cnt_n <- df_cnt_part %>% group_by(country) %>%
        summarise(n = sum(value))

gg_cnt_part <- ggplot(data = df_cnt_part, aes(x = year, y = country))+
        geom_point(color = brbg11[11], size = 7)+
        geom_text(data = d_cnt_n, aes(y = country, x = 17.5, label = n, color = n), size = 7, fontface = 2)+
        geom_text(data = d_cnt_n, aes(y = country, x = 18.5, label = " "), size = 7)+
        scale_color_gradientn(colours = brbg11[7:11])+
        xlab(NULL)+
        ylab(NULL)+
        theme_bw(base_size = 25, base_family = "mono")+
        theme(legend.position = "none", 
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("fig-03-countries-part.png", gg_cnt_part, width = 8, height = 8)




################################################################################
# regression models

# remove small cohorts
table(dfu$cohort)
dfuc <- dfu %>% filter(cohort < 1997, cohort > 1963)

# relevel counrty variable to compare with Russia
dfuc$country <- relevel(dfuc$country, ref = "RUS")


# pulled models
m1 <- lm(data = dfuc, av.height~cohort)
m2 <- lm(data = dfuc, av.height~cohort+position)
m3 <- lm(data = dfuc, av.height~cohort+position+country)

# export the models to html
htmlreg(list(m1, m2, m3), file = "models_height.html", single.row = T)


################################################################################
# players" height by country
gg_av.h_country <- ggplot(dfuc , aes(x = factor(cohort), y = av.height))+
        geom_point(color = "grey50", alpha = .25)+
        stat_summary(aes(group = country), geom = "line", fun.y = mean, size = .5, color = "grey50")+
        stat_smooth(aes(group = country, color = country), geom = "line", size = 1)+
        facet_wrap(~country, ncol = 4)+
        coord_cartesian(ylim = c(170, 195))+
        scale_x_discrete(labels = paste(seq(1970, 1990, 10)), breaks = paste(seq(1970, 1990, 10)))+
        labs(x = "birth cohort", y = "height, cm")+
        theme_few(base_size = 15, base_family = "mono")+
        theme(legend.position = "none", 
              panel.grid = element_line(colour = "grey75", size = .25))

ggsave("fig-04-height-by-country.png", gg_av.h_country, width = 10, height = 12)



# separate models for positions VISUALIZE
dfuc_pos <- dfuc
levels(dfuc_pos$position) <- c("Defenders", "Forwards", "Goalkeeprs")

gg_pos <- ggplot(dfuc_pos , aes(x = cohort, y = av.height))+
        geom_jitter(aes(color = position), alpha = .5, size = 2)+
        stat_smooth(method = "lm", se = T, color = brbg11[11], size = 1)+
        scale_x_continuous(labels = seq(1970, 1990, 10), breaks = seq(1970, 1990, 10))+
        scale_color_manual(values = brbg11[c(8, 9, 10)])+
        facet_wrap(~position, ncol = 3)+
        xlab("birth cohort")+
        ylab("height, cm")+
        theme_few(base_size = 15, base_family = "mono")+
        theme(legend.position = "none", 
              panel.grid = element_line(colour = "grey75", size = .25))

ggsave("fig-05-corr-by-pos.png", gg_pos, width = 12, height = 5)


# separate models for positions
m3d <- lm(data = dfuc %>% filter(position == "D"), av.height~cohort+country)
m3f <- lm(data = dfuc %>% filter(position == "F"), av.height~cohort+country)
m3g <- lm(data = dfuc %>% filter(position == "G"), av.height~cohort+country)
htmlreg(list(m3d, m3f, m3g), file = "models_height_pos.html", single.row = T, 
        custom.model.names = c("Model 3 D", "Model 3 F", "Model 3 G"))



################################################################################
# compare to population


# download the data from Hatton, T. J., & Bray, B. E. (2010). 
# Long run trends in the heights of European men, 19th–20th centuries. 
# Economics & Human Biology, 8(3), 405–413. 
# http://doi.org/10.1016/j.ehb.2010.03.001
# stable URL (https://dx.doi.org/10.6084/m9.figshare.3394795.v1)
df_hb <- read.csv("https://ndownloader.figshare.com/files/5303878") 

df_hb <- df_hb %>%
        gather("country", "h_pop", 2:16) %>%
        mutate(period = paste(period)) %>%
        separate(period, c("t1", "t2"), sep = "/")%>%
        transmute(cohort = (as.numeric(t1)+as.numeric(t2))/2, country, h_pop)

# calculate hockey players' cohort height averages for each country
df_hoc <- dfu %>% group_by(country, cohort) %>%
        summarise(h_hp = mean(av.height)) %>%
        ungroup()


# countries in both data sets
both_cnt <- levels(factor(df_hb$country))[which(levels(factor(df_hb$country)) %in% levels(df_hoc$country))]


gg_hoc_vs_pop <- ggplot()+
        geom_path(data = df_hb %>% filter(country %in% both_cnt), 
                  aes(x = cohort, y = h_pop), 
                  color = brbg11[9], size = 1)+
        geom_point(data = df_hb %>% filter(country %in% both_cnt), 
                   aes(x = cohort, y = h_pop), 
                   color = brbg11[9], size = 2)+
        geom_point(data = df_hb %>% filter(country %in% both_cnt), 
                   aes(x = cohort, y = h_pop), 
                   color = "white", size = 1.5)+
        geom_point(data = df_hoc %>% filter(country %in% both_cnt), 
                   aes(x = cohort, y = h_hp), 
                   color = brbg11[3], size = 2, pch = 18)+
        stat_smooth(data = df_hoc %>% filter(country %in% both_cnt), 
                    aes(x = cohort, y = h_hp), 
                    method = "lm", se = F, color = brbg11[1], size = 1)+
        facet_wrap(~country, ncol = 2)+
        labs(y = "height, cm", x = "birth cohort")+
        theme_few(base_size = 20, base_family = "mono")+
        theme(panel.grid = element_line(colour = "grey75", size = .25))

ggsave("fig-06-players-vs-population.png", gg_hoc_vs_pop, width = 10, height = 12)


# growth in population

df_hb_w <- df_hb %>% spread(cohort, h_pop) 
names(df_hb_w)[2:26] <- paste("y", names(df_hb_w)[2:26])

diffs <- df_hb_w[, 3:26]-df_hb_w[, 2:25]

df_hb_gr<- df_hb_w %>%
        transmute(country, 
                  gr_1961_1980 = unname(apply(diffs[, 22:24], 1, mean, na.rm = T))*2, 
                  gr_1901_1960 = unname(apply(diffs[, 9:21], 1, mean, na.rm = T))*2, 
                  gr_1856_1900 = unname(apply(diffs[, 1:8], 1, mean, na.rm = T))*2) %>%
        gather("period", "average_growth", 2:4) %>%
        filter(country %in% both_cnt) %>%
        mutate(country = factor(country, levels = rev(levels(factor(country)))), 
               period = factor(period, labels = c("1856-1900", "1901-1960", "1961-1980")))


gg_hb_growth <- ggplot(df_hb_gr, aes(x = average_growth, y = country))+
        geom_point(aes(color = period), size = 3)+
        scale_color_manual(values = brbg11[c(8, 3, 10)])+
        scale_x_continuous(limits = c(0, 2.15))+
        facet_wrap(~period)+
        theme_few()+
        xlab("average growth in men's height over 10 years, cm")+
        ylab(NULL)+
        theme_few(base_size = 20, base_family = "mono")+
        theme(legend.position = "none", 
              panel.grid = element_line(colour = "grey75", size = .25))

ggsave("fig-07-av-growth-pop.png", gg_hb_growth, width = 10, height = 4)



################################################################################
# selectivity

# check if there are more players born in earlier months
df_month <- df %>% mutate(month = month(birth)) %>%
        mutate(month = factor(month))

gg_month <- ggplot(df_month, aes(x = factor(month)))+
        geom_bar(stat = "count", fill = brbg11[8])+
        scale_x_discrete(breaks = 1:12, labels = month.abb)+
        labs(x = "month of birth", y = "# of players")+
        theme_few(base_size = 20, base_family = "mono")+
        theme(legend.position = "none", 
              panel.grid = element_line(colour = "grey75", size = .25))

ggsave("fig-08-month-selectivity.png", gg_month, width = 10, height = 6)


# facet by decades
df_month_dec <- df_month %>%
        mutate(dec = substr(paste(cohort), 3, 3) %>% 
                       factor(labels = paste("born in", c("1960s", "1970s", "1980s", "1990s"))))

gg_month_dec <- ggplot(df_month_dec, aes(x = factor(month)))+
        geom_bar(stat = "count", fill = brbg11[8])+
        scale_x_discrete(breaks = 1:12, labels = month.abb)+
        labs(x = "month of birth", y = "# of players")+
        facet_wrap(~dec, ncol = 2, scales = "free")+
        theme_few(base_size = 20, base_family = "mono")+
        theme(legend.position = "none", 
              panel.grid = element_line(colour = "grey75", size = .25))

ggsave("fig-09-month-selectivity-decades.png", gg_month_dec, width = 14, height = 9)



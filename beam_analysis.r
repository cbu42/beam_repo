# Loading libraries
library(tidyverse)
library(scales)
#Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Reading in data as csv files. 
hs_a20 <- read_csv("clean_data/hs_aut20_csv.csv")
hs_w21 <- read_csv("clean_data/hs_win21_csv.csv")
hs_s21 <- read_csv("clean_data/hs_spr21_csv.csv")
eng_a20 <- read_csv("clean_data/eng_aut20_csv.csv")
eng_w21 <- read_csv("clean_data/eng_win21_csv.csv")
eng_s21 <- read_csv("clean_data/eng_spr21_csv.csv")
fs_a20 <- read_csv("clean_data/fs_aut20_csv.csv")
fs_w21 <- read_csv("clean_data/fs_win21_csv.csv")
fs_s21 <- read_csv("clean_data/fs_spr21_csv.csv")

# cleaning/categorizing
#quarter
hs_a20$quarter <- "Autumn"
hs_w21$quarter <- "Winter"
hs_s21$quarter <- "Spring"
eng_a20$quarter <- "Autumn"
eng_w21$quarter <- "Winter"
eng_s21$quarter <- "Spring"
fs_a20$quarter <- "Autumn"
fs_w21$quarter <- "Winter"
fs_s21$quarter <- "Spring"
df$quarter = factor(df$quarter, levels = c("Autumn", "Winter", "Spring"))


#term
hs_a20$term <- "aut20"
hs_w21$term <- "win21"
hs_s21$term <- "spr21"
eng_a20$term <- "aut20"
eng_w21$term <- "win21"
eng_s21$term <- "spr21"
fs_a20$term <- "aut20"
fs_w21$term <- "win21"
fs_s21$term <- "spr21"
#AY
hs_a20$academic_year <- 2021
hs_w21$academic_year <- 2021
hs_s21$academic_year <- 2021
eng_a20$academic_year <- 2021
eng_w21$academic_year <- 2021
eng_s21$academic_year <- 2021
fs_a20$academic_year <- 2021
fs_w21$academic_year <- 2021
fs_s21$academic_year <- 2021

#population
hs_a20$population <- "h&s"
hs_w21$population <- "h&s"
hs_s21$population <- "h&s"
eng_a20$population <- "engineering"
eng_w21$population <- "engineering"
eng_s21$population <- "engineering"
fs_a20$population <- "frosh/soph"
fs_w21$population <- "frosh/soph"
fs_s21$population <- "frosh/soph"

df <- rbind(hs_a20, hs_w21, hs_s21, eng_a20, eng_w21, eng_s21, fs_a20, fs_w21, fs_s21)

init_q_week <- function(df) {
  q <- substring(df$quarter, 1, 1)
  q_week <- paste0(rep(q),df$week)
}
df$q_week <- init_q_week(df)
alevels <- paste0(rep('A',10), c(1:10))
wlevels <- paste0(rep('W',10), c(1:10))
slevels <- paste0(rep('S',10), c(1:10))
qweek_levels <- append(alevels, append(wlevels, slevels, after = length(wlevels)), after = length(alevels))
df$q_week = factor(df$q_week, levels = qweek_levels)

view(df)

#by pop
eng_df <- subset(df, population=='engineering')
fs_df <- subset(df, population=='frosh/soph')
hs_df <- subset(df, population=='h&s')

#means
mean(eng_df$open_rate) #.308
mean(eng_df$click_rate) #.0565
mean(hs_df$open_rate) #.333
mean(hs_df$click_rate) #.0570
mean(fs_df$open_rate) #0.4039751
mean(fs_df$click_rate) #0.08682566

############################## BEGIN AMY PRESENTATION 10/26/21 ##############################

# engineering, click, fullyear
eng_bar_click_fullyear = ggplot(eng_df, aes(x=week, y=click_rate)) +
  geom_col(aes(fill = quarter))+
  geom_text(aes(label = percent(click_rate, accuracy = 1)), vjust = 1.5, colour = "white") +
  facet_wrap(~ quarter) +
  xlab("Week") +
  ylab("Mean Click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  facet_wrap(~quarter) +
  scale_x_continuous(breaks = seq(1:10)) +
  ggtitle('Engineering Click Rate, AY 2021') #+
eng_bar_click_fullyear
ggsave(eng_bar_click_fullyear,
       file = "exports/eng_bar_click_fullyear.png",
       height = 8,
       width = 16)
# engineering, open, fullyear
eng_bar_open_fullyear = ggplot(eng_df, aes(x=week, y=open_rate)) +
  geom_col(aes(fill = quarter))+
  geom_text(aes(label = percent(open_rate, accuracy = 1)), vjust = 1.5, colour = "white") +
  facet_wrap(~ quarter) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  facet_wrap(~quarter) +
  scale_x_continuous(breaks = seq(1:10)) +
  ggtitle('Engineering Open Rate, AY 2021') #+
eng_bar_open_fullyear
ggsave(eng_bar_open_fullyear,
       file = "exports/eng_bar_open_fullyear.png",
       height = 8,
       width = 16)

# h&s, click, fullyear
hs_bar_click_fullyear = ggplot(hs_df, aes(x=week, y=click_rate)) +
  geom_col(aes(fill = quarter))+
  geom_text(aes(label = percent(click_rate, accuracy = 1)), vjust = 1.5, colour = "white") +
  facet_wrap(~ quarter) +
  xlab("Week") +
  ylab("Mean Click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  facet_wrap(~quarter) +
  scale_x_continuous(breaks = seq(1:10)) +
  ggtitle('Humanities & Sciences Click Rate, AY 2021') #+
hs_bar_click_fullyear
ggsave(hs_bar_click_fullyear,
       file = "exports/hs_bar_click_fullyear.png",
       height = 8,
       width = 16)
# h&s, open, fullyear
hs_bar_open_fullyear = ggplot(hs_df, aes(x=week, y=open_rate)) +
  geom_col(aes(fill = quarter))+
  geom_text(aes(label = percent(open_rate, accuracy = 1)), vjust = 1.5, colour = "white") +
  facet_wrap(~ quarter) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  facet_wrap(~quarter) +
  scale_x_continuous(breaks = seq(1:10)) +
  ggtitle('Humanities & Sciences Open Rate, AY 2021') #+
hs_bar_open_fullyear
ggsave(hs_bar_open_fullyear,
       file = "exports/hs_bar_open_fullyear.png",
       height = 8,
       width = 16)

# fs, click, fullyear
fs_bar_click_fullyear = ggplot(fs_df, aes(x=week, y=click_rate)) +
  geom_col(aes(fill = quarter))+
  geom_text(aes(label = percent(click_rate, accuracy = 1)), vjust = 1.5, colour = "white") +
  facet_wrap(~ quarter) +
  xlab("Week") +
  ylab("Mean Click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  facet_wrap(~quarter) +
  scale_x_continuous(breaks = seq(1:10)) +
  ggtitle('Frosh/Soph Click Rate, AY 2021') #+
fs_bar_click_fullyear
ggsave(fs_bar_click_fullyear,
       file = "exports/fs_bar_click_fullyear.png",
       height = 8,
       width = 16)
# fs, open, fullyear
fs_bar_open_fullyear = ggplot(fs_df, aes(x=week, y=open_rate)) +
  geom_col(aes(fill = quarter))+
  geom_text(aes(label = percent(open_rate, accuracy = 1)), vjust = 1.5, colour = "white") +
  facet_wrap(~ quarter) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  facet_wrap(~quarter) +
  scale_x_continuous(breaks = seq(1:10)) +
  ggtitle('Frosh/Soph Open Rate, AY 2021') #+
fs_bar_open_fullyear
ggsave(fs_bar_open_fullyear,
       file = "exports/fs_bar_open_fullyear.png",
       height = 8,
       width = 16)

############################## END AMY PRESENTATION 10/26/21 ##############################


eng_open_scatter = ggplot(filter(df, population=='engineering'), aes(x=week, y=open_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Engineering Open Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
eng_open_scatter


hs_open_scatter = ggplot(filter(df, population=='h&s'), aes(x=week, y=open_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Humanities and Sciences Open Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
hs_open_scatter

fs_open_scatter = ggplot(filter(df, population=='frosh/soph'), aes(x=week, y=open_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Frosh/Soph Open Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
fs_open_scatter

#note: add frosh/soph
#note: make the finance/consulting figures larger
#note: click rate winter AY21 looks inflated
#get her some means

eng_click_scatter = ggplot(filter(df, population=='engineering'), aes(x=week, y=click_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  xlab("Week") +
  ylab("Mean Click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Humanities and Sciences Click Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
eng_click_scatter

hs_click_scatter = ggplot(filter(df, population=='h&s'), aes(x=week, y=click_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  xlab("Week") +
  ylab("Mean Click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Humanities and Sciences Click Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
hs_click_scatter

fs_click_scatter = ggplot(filter(df, population=='frosh/soph'), aes(x=week, y=click_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  xlab("Week") +
  ylab("Mean Click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Frosh/Soph Click Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
fs_click_scatter

eng_click_scatter = ggplot(filter(df, population=='engineering'), aes(x=week, y=click_rate)) +
  geom_point(aes(color=quarter),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Engineering Click Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
eng_click_scatter

all_click_scatter = ggplot(df, aes(x=week, y=click_rate)) +
  geom_point(aes(color=quarter, shape = population),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Click Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
all_click_scatter

all_open_scatter = ggplot(df, aes(x=week, y=open_rate)) +
  geom_point(aes(color=quarter, shape = population),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean click Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Open Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
all_open_scatter


aut_scatter = ggplot(filter(df, quarter=='autumn'), aes(x=week, y=open_rate)) +
  geom_point(aes(color=population),size=2.5) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Humanities and Sciences Open Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") +
  geom_smooth(method='lm')
eng_scatter
mean(subset(df, quarter=='autumn')$open_rate)

# keep this scatterplot as is (no f/s)
scatter = ggplot(df, aes(x=week, y=open_rate, color=quarter, shape=population)) +
  geom_point(aes(color=quarter),size=2.5) +
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Week") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Open Rate, AY 2021') +
  scale_x_continuous(breaks = seq(1:10)) +
  abline(v=1:10, col="gray", lty="dotted") 
#geom_smooth(method='lm')
scatter

?facet_grid



#include outlier. why: we need to tackle the mass-unsub problem. ppl dont get newsletters until week 2.
plot(total_recipients ~ week, aut19, pch = 19, main = "Autumn 2019, Total Recipients over time")
axis(side=1, at=c(1:10))
abline(v=1:10, col="gray", lty="dotted")
#remove outlier for clear view
plot(total_recipients ~ week, aut19[-c(1),], pch = 19, main = "Autumn 2019, Total Recipients over time")
axis(side=1, at=c(1:10))
abline(h=mean(aut19[-c(1), ]$total_recipients), col="blue")
text(4.5, 68.4, "a")
abline(v=1:10, col="gray", lty="dotted")

?geom_bar()

ggplot(aut19, aes(aut19$week, aut19$total_recipients), xlab = "week", ylab = "total recipients") +
  geom_col() +
  theme_minimal_grid(12)

aut19clean <- aut19[-c(1),]
aut20clean <- aut20[-c(1),]

?geom_abline

t.test(x,y)


abline(h=mean(aut19clean$total_recipients), col="blue", )+
  abline(v=2:10, col="gray", lty="dotted")

##AAAHHH
plot19 <- ggplot(aut19clean, aes(week, total_recipients), xlab = 'week', ylab = "total recipients") + 
  geom_point() +
  geom_text(aes(label=aut19clean$total_recipients),hjust=0, vjust=0) +
  geom_abline(slope = 0, intercept =  mean(total_recipients), col = "blue")
(plot19)
plot20 <- ggplot(aut20clean, aes(week, total_recipients), xlab = 'week', ylab = "total recipients") + 
  geom_point() +
  geom_text(aes(label=aut20clean$total_recipients),hjust=0, vjust=0) +
  geom_abline(slope = 0, intercept =  mean(aut20clean$total_recipients), col = "blue")
plot_grid(plot19, plot20, labels = c('Aut 19', 'Aut 20'), label_size = 12)



#Aut20
plot(total_recipients ~ week, aut20, pch = 19, main = "Autumn 2020, Total Recipients over time")
axis(side=1, at=c(1:10))
abline(h=mean(aut20$total_recipients), col="blue", )
abline(v=1:10, col="gray", lty="dotted")

#OPENS
plot(unique_opens ~ week, eng_a20[-c(1),], pch = 19, main = "Autumn 2019, Unique Opens over time")
axis(side=1, at=c(1:10))
abline(h=mean(aut19[-c(1), ]$unique_opens), col="blue", )
abline(v=1:10, col="gray", lty="dotted")

plot(unique_opens ~ week, aut20, pch = 19, main = "Autumn 2020, Unique Opens over time")
axis(side=1, at=c(1:10))
abline(h=mean(aut20$unique_opens), col="blue", )
abline(v=1:10, col="gray", lty="dotted")


#OPEN RATE
plot(open_rate ~ week, aut19[-c(1),], pch = 19, main = "Autumn 2019, Open Rate over time")
axis(side=1, at=c(1:10))
abline(h=mean(aut19[-c(1), ]$open_rate), col="blue" )
abline(v=1:10, col="gray", lty="dotted")

plot(open_rate ~ week, aut20, pch = 19, main = "Autumn 2020, Open Rate over time")
axis(side=1, at=c(1:10))
abline(h=mean(aut20$open_rate), col="blue", )
abline(v=1:10, col="gray", lty="dotted")

view(aut19)


mean(aut19[-c(1), ]$total_recipients)

mean(aut19[-c(1), ]$click_rate)

mean(aut20$open_rate)

mean(aut19[-c(1), ]$unique_opens)
mean(aut20$unique_opens)


mean(aut20$click_rate)


?abline



#the complement of indices 7 and 11 is simply aut19 but numeric
plot_correlation(aut19[,-c(7,11)], type = 'continuous')
corrp_aut19 <- rcorr(as.matrix(aut19[,-c(7,11)]))
(corrp_aut19)


#prioritize aut20. engineering and frosh-soph!
#click rate corr
#open rate



#click rate sauce
(corr_aut19[,c(1,6)])

(corr_aut19[,c(1,6)])


#gather arguments for archives
#y so many new recipients???


###AUTUMN 2020
getwd()
aut20 <- read_csv("aut2020_csv.csv")
view(aut20)
?ggplot()


ggplot(xlab = "week", ylab = "total recipients") + 
  geom_point(data=aut19[-c(1),], aes(x=aut19[-c(1),]$week, y=aut19[-c(1),]$total_recipients), color='blue') + 
  geom_point(data=aut20[-c(1),], aes(x=aut20[-c(1),]$week, y=aut20[-c(1),]$total_recipients), color='red') +
  scale_x_discrete(breaks = seq(1, 10, by = 1)) +
  theme_minimal_grid(12)

ggplot() + 
  geom_line(data=aut19, aes(x=aut19$week, y=aut19$total_recipients), color='green') + 
  geom_line(data=aut20, aes(x=aut20$week, y=aut20$total_recipients), color='red') +
  scale_x_discrete(breaks = seq(1, 10, by = 1))


dataB <- dataA[, c("P1", "xyz", "acdc")]

head(aut19)
head(aut20)

mean(corr_aut19)



#remove week 1 of Aut19 because it's an outlier
aut19_no_outlier <- aut19[-c(1), ] 
head(aut19_no_outlier)
#we will also remove week 1 of Aut20 for comparison of the quarters
aut20_no_outlier <- aut20[-c(1), ] 
head(aut20_no_outlier)
#get the means aut19
head(aut19)
aut19means <- colMeans(aut19_no_outlier[ , c(2, 3, 4, 5, 7, 8, 9)])
head(aut19means)

#means
head(aut20)
aut20means <- colMeans(aut20_no_outlier[ , c(2, 3, 4, 5, 6, 8, 9, 10)])
head(aut20means)

#creating dual-quarter df
aut_compare <- aut19[]



qplot(x = aut19, y = aut20,
      geom = "boxplot", data = birthwt,
      xlab = "population", 
      ylab = "clickrate",
      fill = I("lightblue"))
qplot(aut19$week, aut19$total_recipients)


#base r multiple plots

plot(data$x,data$y1,type="l",main="Normal Distribution",xlab="x",ylab="y")
lines(data$x,data$y2,lty=2,lwd=2,col="green")
lines(data$x,data$y3,lty=3,lwd=2,col="blue")


#label ggplot
geom_text(aes(label=Name),hjust=0, vjust=0)


#finance consulting
fc <- read_csv("mailchimp/cf_csv.csv")

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

fc_plot = ggplot(fc, aes(x=send_date, y=open_rate)) +
  geom_col() +
  geom_text(aes(label = percent(open_rate)), vjust = 1.5, colour = "white") +
  #geom_bar(aes(send_date, open_rate)) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Date") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Finance and Consulting Newsletter Open Rate, AY 2021') #+
  #scale_x_discrete(breaks = seq(1:7)) 
  #abline(v=1:10, col="gray", lty="dotted") +
  #geom_smooth(method='lm')
fc_plot
view(fc)

fc_click_plot = ggplot(fc, aes(x=send_date, y=click_rate)) +
  geom_col() +
  geom_text(aes(label = percent(click_rate)), vjust = 1.5, colour = "white") +
  #geom_bar(aes(send_date, open_rate)) +
  #facet_grid(cond ~ pragContext ) + 
  #scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Date") +
  ylab("Mean Open Rate") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  ggtitle('Finance and Consulting Newsletter Click Rate, AY 2021') #+
#scale_x_discrete(breaks = seq(1:7)) 
#abline(v=1:10, col="gray", lty="dotted") +
#geom_smooth(method='lm')
fc_click_plot






####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Data Scientist's History of Buggy
###
### Purpose
###  * Analyses and visualization for the talk "A Data-Scientist's History of
###    Buggy" at Racecay 2015.
###
### Notes:
###  * The slides from that talk can be found here: https://prezi.com/3ghmo_xctamd/
###
### Primary Creator(s): Sam Swift (samswift@cmubuggy.org)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(reshape2)
library(scales)
source("util/query_db.R")
source("util/time.R")

kPlotDir    <- file.path("~","Desktop")
kPlotWidth  <- 8
kPlotHeight <- 6

kScaleColorGender <- c("#0571b0","#ca0020")

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get readable data from DB ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

times <- QueryDB(
  "SELECT raceyear.`year`,
    raceclass.`description` AS raceclass,
    org.`shortname` AS org,
    entry.`place`,
    entry.`abcd`,
    team.`time`,
    raceday.`isfinals`
  FROM team
  JOIN `entry`
    ON entry.`entryid` = team.`entryid`
  JOIN `org`
    ON entry.`orgid` = org.`orgid`
  JOIN `raceyear`
    ON raceyear.`raceyearid` = entry.`raceyearid`
  JOIN `raceday`
    ON raceday.`racedayid` = team.`racedayid`
  JOIN `raceclass`
    ON raceclass.`raceclassid` = entry.`raceclassid`
  ORDER BY raceclass, year, place")
times[, year   := as.integer(year)]
times[, decade := floor(year/10)*10]

entries <- QueryDB(
  "SELECT
    raceyearid AS year,
    org.`shortname` as org,
    org.isgreek,
    abcd,
    raceclass.`description` AS raceclass
  FROM entry
  JOIN `org`
    ON entry.`orgid` = org.`orgid`
  JOIN `raceclass`
    ON raceclass.`raceclassid` = entry.`raceclassid`;")



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Participation ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# count the number of entries by each org each year
org.year <- entries[, list(n_teams = .N,
                           n_men = sum(raceclass == "Men's"),
                           n_women = sum(raceclass == "Women's")),
                    by=list(year, org, isgreek)]

# normalized by the number of entries each year
org.year[, share_teams := n_teams / sum(n_teams), by= year]
setkey(org.year, year)

# plot total number of entries each year
setkeyv(entries, c("raceclass","isgreek"))
ggplot(entries[year >= 1976,
               list(n_teams = .N,
                    is_greek = unique(isgreek) == 1),
               by=list(year,org,Gender = raceclass)],
       aes(year, n_teams, fill=Gender,
           alpha=factor(is_greek,
                        levels=c( TRUE,FALSE),
                        labels=c("Greek","Independent"), ordered = TRUE))) +
  geom_bar(stat = "identity", position="stack") +
  scale_alpha_discrete(range = c(1,.6))+
  scale_fill_manual(values = kScaleColorGender) +
  labs(x="",y="Number of Teams", title="Total Participation in Buggy", color="", alpha="") +
  theme_bw(base_size=16)
ggsave(filename = file.path(kPlotDir, "participation.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

# plot gender trend in participation
ggplot(melt(entries[order(year),
               list(per_women = sum(raceclass == "Women's")/.N,
                    per_men   = sum(raceclass == "Men's")/.N),
               by = year],
            id.vars = "year", value.name = "percent", variable.name = "gender"),
       aes(year, percent, fill=gender, alpha = gender)) +
  geom_bar(position="stack", stat="identity") +
  geom_hline(yintercept = .5) +
  scale_fill_manual(values = rev(kScaleColorGender)) +
  scale_alpha_manual(values = c(1,.3)) +
  scale_x_continuous(breaks=seq(1920,2015,5)) +
  scale_y_continuous(labels=percent)+
  labs(x="",y="Percent of Pushers", title = "Percentage of Female Push Teams by Year") +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = file.path(kPlotDir, "participation.gender.png"),
       width = kPlotWidth,
       height = kPlotHeight)

# summarize overall stats for each org
orgs <- entries[year > 1965,
                list(n_entries = .N,
                       n_years = length(unique(year)),
                       n_entries_year = .N / length(unique(year)),
                       active = max(year) == 2014,
                       first_year = min(year),
                       last_year = max(year)),
                by=org]

# merge org stats so we can sort by them
org.year <- merge(org.year, orgs[, list(org, active)], by="org", all.x=T)
org.year[, org := factor(org, levels = orgs[order(first_year,decreasing = TRUE), org])]

# plot history of org participation and size
ggplot(org.year, aes(year, org, color=org, fill=org)) +
  geom_line()+
  geom_point(aes(size = n_teams),shape=21, color="white") +
  scale_size_continuous(range = c(3,10)) +
  scale_x_continuous(breaks=seq(1920,2015,5)) +
  labs(x="",y="", title="Buggy Org Participation History") +
  theme_bw(base_size=20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

ggsave(filename = file.path(kPlotDir, "participation.org.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Competitiveness ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get the best time for each org and each gender in each year
top.times <- times[time != 0,
                   list(best_time = min(time)),
                   by=list(year, raceclass, org)]
setkeyv(top.times, c("year", "raceclass", "best_time"))

# rank the orgs in each year
top.times[, rank := 1:.N, by=list(year, raceclass)]

# how far was each org from winning?
top.times[, gap_win  := best_time - min(best_time), by=list(year, raceclass)]

# how far was each org from moving up one rank
top.times[, gap_next := c(0,diff(best_time)), by=list(year, raceclass)]

# plot the best time of each year
ggplot(top.times[rank == 1 & year > 1922],
       aes(year, best_time,
           color=raceclass)) +
  geom_path(aes(group = factor(rank))) +
  scale_color_manual(values = kScaleColorGender)+
  scale_x_continuous(breaks = seq(1920,2015,5))+
  scale_y_continuous(breaks=seq(120,240,10),
                     labels=SecondsToString(seq(120,240,10))) +
  labs(x="",y="Time",title="Best Time By Year") +
  facet_grid(raceclass~.,scales = "free_y",space = "free_y") +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = file.path(kPlotDir, "perf.best.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

# plot the top N times from each year
ggplot(top.times[year > 1969 & rank <= 8],
       aes(year, best_time,
           color=factor(rank))) +
  geom_path(aes(group = factor(rank))) +
  scale_x_continuous(breaks = seq(1920,2015,5))+
  scale_y_continuous(breaks=seq(120,240,10),
                     labels=SecondsToString(seq(120,240,10))) +
  labs(x="",y="Time",title="Top 8 Times By Year", color="Rank") +
  facet_grid(raceclass~.,scales = "free_y",space = "free_y") +
  theme_bw()

ggsave(filename = file.path(kPlotDir, "perf.top8.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

# plot the margins of victory
ggplot(top.times[year > 1969 & between(rank,2,5)],
       aes(year, gap_win,
           color=factor(rank))) +
  geom_point(aes(group = factor(rank)), size=3) +
  geom_point(aes(group = factor(rank)), shape=21, size=3, color="black") +
  geom_smooth(aes(group=1), method="loess") +
  scale_x_continuous(breaks = seq(1920,2015,5))+
  #scale_color_brewer(type="seq", palette="Blues")+
  scale_color_manual(values=rev(brewer.pal(4, "Blues"))) +
  labs(x="",y="Seconds Behind Winner",title="Margins of Victory by Year", color="Rank") +
  facet_grid(raceclass~.,scales = "free_y",space = "free_y") +
  theme_bw()

ggsave(filename = file.path(kPlotDir, "perf.margin.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

percentiles <- melt(times[time >0 & abcd =="A" & year >=1970,
                          as.list(quantile(time,
                                           seq(.00,.99,.01))),
                          by=list(raceclass,decade)],
                    id.vars = c("raceclass","decade"),
                    variable.name="percentile",
                    value.name="time_cutoff")
percentiles[, percentile := 1-(as.numeric(percentile)/100)]
ggplot(percentiles,
       aes(percentile, time_cutoff, color=raceclass, alpha=decade)) +
  scale_color_manual(values = kScaleColorGender)+
  scale_y_continuous(breaks=seq(120,240,10),
                     labels=SecondsToString(seq(120,240,10))) +
  scale_x_continuous(labels=percent)+
  geom_line(aes(group=decade)) +
  facet_grid(raceclass~., scales="free_y") +
  labs(x="Percentile", y="Time", color="Gender", title="Percentiles Thresholds for A teams by Decade") +
  theme_bw()

ggsave(filename = file.path(kPlotDir, "perf.percentiles.png"),
       width = kPlotWidth,
       height = kPlotHeight)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Eras of innovation, Eras of stagnation
# change in winning time
# normalized for asymtotic performance barrier
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RunningMin <- function(times){
  mins <- c()
  for(i in 1:length(times)){
     mins[i] <- min(times[1:i])
  }
  return(mins)
}

top.times[, record := RunningMin(best_time), by=raceclass]
top.times[, new_record := best_time == record]

# plot record in each year with new records denoted
ggplot(top.times[year > 1922],
       aes(year, record,
           color=raceclass)) +
  geom_point() +
  geom_point(data=top.times[year > 1922 & best_time < 198,
                            list(best_time = min(best_time)),
                            by=list(year,raceclass)],
             aes(year, best_time), shape=21) +
  geom_segment(data = top.times[new_record == TRUE & year > 1922],
             aes(x = year, xend = year,
                 y = max(record), yend = record,
                 color=raceclass)) +
    scale_color_manual(values = kScaleColorGender)+
  scale_x_continuous(breaks = seq(1925,2015,5))+
  scale_y_continuous(breaks=seq(120,240,10),
                     labels=SecondsToString(seq(120,240,10))) +
  labs(x="",y="Time",title="Record Time and New Records by Year") +
  facet_grid(raceclass~.,scales = "free_y",space = "free_y") +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = file.path(kPlotDir, "comp.record.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

# show how the trend of improvement is asymptotic
ggplot(top.times[year > 1922],
       aes(year, record,
           color=raceclass)) +
  geom_point() +
  stat_smooth(method="lm",
              formula = y~poly(x,2)) +
  scale_color_manual(values = kScaleColorGender)+
  scale_x_continuous(breaks = seq(1925,2015,5))+
  scale_y_continuous(breaks=seq(120,240,10),
                     labels=SecondsToString(seq(120,240,10))) +
  labs(x="",y="Time",title="Asymptotic Record Times") +
  facet_grid(raceclass~.,scales = "free_y",space = "free_y") +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = file.path(kPlotDir, "comp.asym.year.png"),
       width = kPlotWidth,
       height = kPlotHeight)

top.times[raceclass == "Men's", asym := 120]
top.times[raceclass == "Women's", asym := 140]

top.times[new_record == TRUE, record_delta := c(NA,abs(diff(record))), by=raceclass]
top.times[new_record == TRUE, remaining_gap := record - asym, by=raceclass]
top.times[new_record == TRUE, percent_gap := record_delta / (remaining_gap + record_delta)]

ggplot(top.times, aes(year, -percent_gap, fill = raceclass)) +
  geom_bar(stat="identity") +
  facet_grid(raceclass~.) +
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks = seq(1925,2015,5))+
  scale_fill_manual(values = kScaleColorGender)+
  labs(x="", y="Percent of remaining delta",
       title="New Records as Progress Toward Potential Asymptote") +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = file.path(kPlotDir, "comp.asym.progress.png"),
       width = kPlotWidth,
       height = kPlotHeight)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Dynasties
# org ascendency
# Most dominant dynasties

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Freerolls
# weather
# schedule
# consitency (lack of DQs and spins)










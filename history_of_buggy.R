####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Data Scientist's History of Buggy
###
### Purpose
###  *
###
### Notes:
###  *
###
### Primary Creator(s): Sam Swift (swift@betterment.com)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(ggplot2)
source("util/query_db.R")

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

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Most Competitive Eras ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

top.times <- times[time != 0,
                   list(best_time = min(time)),
                   by=list(year, raceclass, org)]

setkeyv(top.times, c("year", "raceclass", "best_time"))

top.times[, rank := 1:.N, by=list(year, raceclass)]

top.times[, gap_win  := best_time - min(best_time), by=list(year, raceclass)]
top.times[, gap_next := c(0,diff(best_time)), by=list(year, raceclass)]

ggplot(top.times[year > 1969 & rank <= 8],
       aes(year, best_time,
           color=factor(rank))) +
  geom_path(aes(group = factor(rank))) +
  facet_grid(raceclass~.,scales = "free_y",space = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(top.times[year > 1969 & between(rank,2,4)],
       aes(year, gap_win,
           color=factor(rank))) +
  geom_point(aes(group = factor(rank))) +
  geom_smooth(aes(group=1))+
  facet_grid(raceclass~.,scales = "free_y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### org longgevity / greek vs ind
# org ascendency
# Most dominant dynasties
### Number of wins and average margin of victory ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

org.year <- entries[, list(n_teams = .N,
                           n_men = sum(raceclass == "Men's"),
                           n_women = sum(raceclass == "Women's")),
                    by=list(year, org, isgreek)]

org.year[, share_teams := n_teams / sum(n_teams), by= year]

setkey(org.year, year)

orgs <- entries[, list(n_entries = .N,
                       n_years = length(unique(year)),
                       n_entries_year = .N / length(unique(year)),
                       active = max(year) == 2014,
                       first_year = min(year),
                       last_year = max(year)),
                by=org]

org.year <- merge(org.year, orgs[, list(org, active)], by="org", all.x=T)
org.year[, org := factor(org, levels = orgs[order(first_year,decreasing = TRUE), org])]

ggplot(org.year, aes(year, org, color=org, fill=org)) +
  geom_line()+
  geom_point(aes(size = n_teams),shape=21, color="white") +
  scale_size_continuous(range = c(3,10)) +
  scale_x_continuous(breaks=seq(1920,2015,5)) +
  labs(x="",y="", title="Buggy Org Participation History") +
  theme_bw(base_size=20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")



org.year[org %in% orgs[fir]]

### IDEAS

# Most competitive eras
# change in the spread between top 6 times

# Eras of innovation, Eras of stagnation
# change in winning time

# number of freerolls, first

# Athletic vs engineering

# build frequency over time

# consitency (lack of DQs and spins)

# number of entries, org depth

# historic numbers of entries, pushers, teams, etc










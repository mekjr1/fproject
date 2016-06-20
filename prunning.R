
require(RSQLite)


#probe the database to get all relevant subreddits' comments (nba, ncca, basketball,collegeBasketball)
db <- src_sqlite("database.sqlite", create = F)
df <- db %>% tbl("May2015") %>%
  filter(subreddit %in% c("nba","ncca","basketball","CollegeBasketball")) %>%
  collect()

#all the retrieved comment lines are retrived and save in  nbasubredditfull.csv
#this is just a preventive mesure in case something goes wrong so we dont have to go through de whole database again
write.csv(df,"nbasubredditfull.csv")
 
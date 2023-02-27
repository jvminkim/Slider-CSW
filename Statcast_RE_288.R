#Pulling Statcast pitches(2017-2022) from SQL Server
query_pitch_type <- "
SELECT *
FROM pitch_events
"

conn <- dbConnect(MySQL(), dbname = "Statcast",
                  user = "root", password = ":)")

statCast <- dbGetQuery(conn, query_pitch_type)

statCastDat <- statCast
#Creating state and unique pitch identifiers
statCastDat <- statCastDat %>%
  mutate(PITCH_ID = paste(game_pk, at_bat_number, pitch_number),
         STATE = paste(ifelse(is.na(on_1b),0,1), ifelse(is.na(on_2b),0,1), ifelse(is.na(on_3b),0,1), sep = ""),
         OUTS = outs_when_up,
         RUNS = home_score + away_score,
         COUNT = paste(balls, strikes, sep = "-"),
         HALF.INNINGS = paste(game_pk, inning, inning_topbot))

#Ordering by game, at bat and pitch in at bat
statCastDat <- statCastDat[order(statCastDat$game_pk,statCastDat$at_bat_number,statCastDat$pitch_number),]
statCastDat$ID <- seq_len(nrow(statCastDat))
statCastDat$NXT_ID <- statCastDat$ID + 1

#Identifying next pitch and state details
statCastDat_RE_NXT <- statCastDat[c("PITCH_ID","STATE","OUTS","RUNS","COUNT","HALF.INNINGS","ID")]
oldnames = c("PITCH_ID","STATE","OUTS","RUNS","COUNT","HALF.INNINGS","ID")
newnames = c("NEW_PITCH_ID","NEW_STATE","NEW_OUTS","NEW_RUNS","NEW_COUNT","NEW_HALF.INNINGS","ID")
statCastDat_RE_NXT %>% rename_at(vars(oldnames), ~ newnames) -> statCastDat_RE_NXT
statCastDat_RE_M <- merge(statCastDat, statCastDat_RE_NXT, by = "NXT_ID", by.y = "ID", all = TRUE)

statCastDat_RE_M$NEW_OUTS <- ifelse(statCastDat_RE_M$HALF.INNINGS != statCastDat_RE_M$NEW_HALF.INNINGS 
                                  & str_sub(statCastDat_RE_M$HALF.INNINGS,1,6) == str_sub(statCastDat_RE_M$NEW_HALF.INNINGS,1,6) 
                                  ,3, statCastDat_RE_M$NEW_OUTS)

statCastDat_RE_M$NEW_OUTS <- ifelse(statCastDat_RE_M$HALF.INNINGS != statCastDat_RE_M$NEW_HALF.INNINGS 
                                  & str_sub(statCastDat_RE_M$HALF.INNINGS,1,6) != str_sub(statCastDat_RE_M$NEW_HALF.INNINGS,1,6) 
                                  & str_sub(statCastDat_RE_M$HALF.INNINGS,-3,-1) == "Top"
                                  ,3, statCastDat_RE_M$NEW_OUTS)

statCastDat_RE_M$PITCH_RUNS <- ifelse(statCastDat_RE_M$NEW_RUNS - statCastDat_RE_M$RUNS < 0 ,0, statCastDat_RE_M$NEW_RUNS - statCastDat_RE_M$RUNS)

statCastDat_RE_HALF <- statCastDat_RE_M %>%
  group_by(HALF.INNINGS) %>%
  summarize(MAX_RUNS = max(RUNS),
            MAX_OUTS = max(NEW_OUTS))

statCastDat_RE_ALL <- merge(statCastDat_RE_M, statCastDat_RE_HALF, by = "HALF.INNINGS")

statCastDat_RE_ALL$ROI <- statCastDat_RE_ALL$MAX_RUNS - statCastDat_RE_ALL$RUNS


statCastDat_RE_CHANGE <- subset(statCastDat_RE_ALL, (paste(STATE,OUTS,COUNT) != paste(STATE,NEW_OUTS,NEW_COUNT)) | PITCH_RUNS > 0)

statCastDat_RE_288 <- statCastDat_RE_CHANGE %>%
  group_by(STATE, OUTS, COUNT) %>%
  filter(substr(COUNT,1,2) != '4') %>%
  filter(MAX_OUTS == 3) %>%
  summarize(RE = mean(ROI))

statCastDat_RE_288 <- statCastDat_RE_288 %>%
  filter(substr(COUNT,1,1) != '4') %>%
  filter(substr(COUNT,3,3) != '3')

statCastDat_RE_288 <- statCastDat_RE_288 %>%
  arrange(COUNT, OUTS, STATE)

statcast_RE <- matrix(round(statCastDat_RE_288$RE, 2), 24, 12) 

dimnames(statcast_RE)[[2]] <- c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
dimnames(statcast_RE)[[1]] <- c("-- -- -- 0", "-- -- 3B 0", "-- 2B -- 0", "-- 2B 3B 0", "1B -- -- 0", "1B -- 3B 0", "1B 2B -- 0", "1B 2B 3B 0",
                             "-- -- -- 1", "-- -- 3B 1", "-- 2B -- 1", "-- 2B 3B 1", "1B -- -- 1", "1B -- 3B 1", "1B 2B -- 1", "1B 2B 3B 1", 
                             "-- -- -- 2", "-- -- 3B 2", "-- 2B -- 2", "-- 2B 3B 2", "1B -- -- 2", "1B -- 3B 2", "1B 2B -- 2", "1B 2B 3B 2")

desired_column_order <- c("0-2", "1-2", "0-1", "2-2", "1-1", "0-0", "1-0", "2-1", "3-2", "2-0", "3-1", "3-0")
desired_row_order <- c("-- -- -- 0", "1B -- -- 0", "-- 2B -- 0", "1B 2B -- 0", "-- -- 3B 0", "1B -- 3B 0", "-- 2B 3B 0", "1B 2B 3B 0",
                       "-- -- -- 1", "1B -- -- 1", "-- 2B -- 1", "1B 2B -- 1", "-- -- 3B 1", "1B -- 3B 1", "-- 2B 3B 1", "1B 2B 3B 1",
                       "-- -- -- 2", "1B -- -- 2", "-- 2B -- 2", "1B 2B -- 2", "-- -- 3B 2", "1B -- 3B 2", "-- 2B 3B 2", "1B 2B 3B 2")

statcast_RE <- statcast_RE[desired_row_order, desired_column_order]

slider_statcastDat <- statCast

slider_statcastDat <- slider_statcastDat %>%
  mutate(PITCH_ID = paste(game_pk, at_bat_number, pitch_number),
         STATE = paste(ifelse(is.na(on_1b),0,1), ifelse(is.na(on_2b),0,1), ifelse(is.na(on_3b),0,1), sep = ""),
         OUTS = outs_when_up,
         RUNS = home_score + away_score,
         COUNT = paste(balls, strikes, sep = "-"),
         HALF.INNINGS = paste(game_pk, inning, inning_topbot))

slider_statcastDat <- slider_statcastDat[order(slider_statcastDat$game_pk,slider_statcastDat$at_bat_number,slider_statcastDat$pitch_number),]
slider_statcastDat$ID <- seq_len(nrow(slider_statcastDat))
slider_statcastDat$NXT_ID <- slider_statcastDat$ID + 1

slider_statcastDat_RE_NXT <- slider_statcastDat[c("PITCH_ID","STATE","OUTS","RUNS","COUNT","HALF.INNINGS","ID", "pitch_type")]
oldnames = c("PITCH_ID","STATE","OUTS","RUNS","COUNT","HALF.INNINGS","ID", "pitch_type")
newnames = c("NEW_PITCH_ID","NEW_STATE","NEW_OUTS","NEW_RUNS","NEW_COUNT","NEW_HALF.INNINGS","ID", "NEW_PITCH_TYPE")
slider_statcastDat_RE_NXT %>% rename_at(vars(oldnames), ~ newnames) -> slider_statcastDat_RE_NXT
slider_statcastDat_RE_M <- merge(slider_statcastDat, slider_statcastDat_RE_NXT, by = "NXT_ID", by.y = "ID", all = TRUE)

slider_statcastDat_RE_M$NEW_OUTS <- ifelse(slider_statcastDat_RE_M$HALF.INNINGS != slider_statcastDat_RE_M$NEW_HALF.INNINGS 
                                    & str_sub(slider_statcastDat_RE_M$HALF.INNINGS,1,6) == str_sub(slider_statcastDat_RE_M$NEW_HALF.INNINGS,1,6) 
                                    ,3, slider_statcastDat_RE_M$NEW_OUTS)

slider_statcastDat_RE_M$NEW_OUTS <- ifelse(slider_statcastDat_RE_M$HALF.INNINGS != slider_statcastDat_RE_M$NEW_HALF.INNINGS 
                                    & str_sub(slider_statcastDat_RE_M$HALF.INNINGS,1,6) != str_sub(slider_statcastDat_RE_M$NEW_HALF.INNINGS,1,6) 
                                    & str_sub(slider_statcastDat_RE_M$HALF.INNINGS,-3,-1) == "Top"
                                    ,3, slider_statcastDat_RE_M$NEW_OUTS)

slider_statcastDat_RE_M$PITCH_RUNS <- ifelse(slider_statcastDat_RE_M$NEW_RUNS - slider_statcastDat_RE_M$RUNS < 0 ,0, slider_statcastDat_RE_M$NEW_RUNS - slider_statcastDat_RE_M$RUNS)

slider_statcastDat_RE_HALF <- slider_statcastDat_RE_M %>%
  filter(NEW_PITCH_TYPE == "SL") %>%
  group_by(HALF.INNINGS) %>%
  summarize(MAX_RUNS = max(RUNS),
            MAX_OUTS = max(NEW_OUTS))

slider_statcastDat_RE_ALL <- merge(slider_statcastDat_RE_M, slider_statcastDat_RE_HALF, by = "HALF.INNINGS")

slider_statcastDat_RE_ALL$ROI <- slider_statcastDat_RE_ALL$MAX_RUNS - slider_statcastDat_RE_ALL$RUNS

slider_statcastDat_RE_CHANGE <- subset(slider_statcastDat_RE_ALL, (paste(STATE,OUTS,COUNT) != paste(STATE,NEW_OUTS,NEW_COUNT)) | PITCH_RUNS > 0)

slider_statcastDat_RE_288 <- slider_statcastDat_RE_CHANGE %>%
  group_by(STATE, OUTS, COUNT) %>%
  filter(substr(COUNT,1,2) != '4') %>%
  filter(MAX_OUTS == 3) %>%
  summarize(RE = mean(ROI))

slider_statcastDat_RE_288 <- slider_statcastDat_RE_288 %>%
  filter(substr(COUNT,1,1) != '4') %>%
  filter(substr(COUNT,3,3) != '3')

slider_statcastDat_RE_288 <- slider_statcastDat_RE_288 %>%
  arrange(COUNT, OUTS, STATE)


slider_RE <- matrix(round(slider_statcastDat_RE_288$RE, 2), 24, 12) 

dimnames(slider_RE)[[2]] <- c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
dimnames(slider_RE)[[1]] <- c("-- -- -- 0", "-- -- 3B 0", "-- 2B -- 0", "-- 2B 3B 0", "1B -- -- 0", "1B -- 3B 0", "1B 2B -- 0", "1B 2B 3B 0",
                              "-- -- -- 1", "-- -- 3B 1", "-- 2B -- 1", "-- 2B 3B 1", "1B -- -- 1", "1B -- 3B 1", "1B 2B -- 1", "1B 2B 3B 1", 
                              "-- -- -- 2", "-- -- 3B 2", "-- 2B -- 2", "-- 2B 3B 2", "1B -- -- 2", "1B -- 3B 2", "1B 2B -- 2", "1B 2B 3B 2")

desired_column_order <- c("0-2", "1-2", "0-1", "2-2", "1-1", "0-0", "1-0", "2-1", "3-2", "2-0", "3-1", "3-0")
desired_row_order <- c("-- -- -- 0", "1B -- -- 0", "-- 2B -- 0", "1B 2B -- 0", "-- -- 3B 0", "1B -- 3B 0", "-- 2B 3B 0", "1B 2B 3B 0",
                       "-- -- -- 1", "1B -- -- 1", "-- 2B -- 1", "1B 2B -- 1", "-- -- 3B 1", "1B -- 3B 1", "-- 2B 3B 1", "1B 2B 3B 1",
                       "-- -- -- 2", "1B -- -- 2", "-- 2B -- 2", "1B 2B -- 2", "-- -- 3B 2", "1B -- 3B 2", "-- 2B 3B 2", "1B 2B 3B 2")

slider_RE <- slider_RE[desired_row_order, desired_column_order]


difference_re <- statcast_RE - slider_RE


difference_re_df <- as.data.frame(difference_re)








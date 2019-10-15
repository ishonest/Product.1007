source("./Functions/F.Trading.Simulation.R")
overview <- readRDS("./Data/Summary/Overview.rds") %>% 
            select(ticker, Exchange, tickerID) %>% distinct()

Prod.20191007 <- function(ticker)
{
  # ticker = "MTLS"
  d1 <- all.d1 %>% filter(ticker == !!ticker) %>%
        select(ds, volume, open, low, high, close) %>% arrange(ds) %>%
        mutate(ROI.l = round(-zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close), 4)
               , ROI.h = round(zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close), 4)
               , ROI.c = round(lead(close, 5-1)/lag(close), 4)
               , ROI.d = close/lag(close)
               )
  
  all <- readRDS(paste0("./Data/Scores/", ticker, ".rds")) %>%
        left_join(d1, by = "ds") %>% mutate(Score = round(Score, 4))

  mods <- prod.models %>% filter(algoId == "20191007", ticker == !!ticker) %>% select(-algoId)
  
  if(is.null(all) || nrow(all) == 0 || is.null(mods) || nrow(mods) == 0)
  {
    rm(ticker, d1, all, mods)
    return(NULL)
  }

  # Binning in Ranges: 10 Bins
  bins <- 10
  sim <- semi_join(all, mods, by = "ID") %>% group_by(ID) %>%
          filter(ds <= Parms[["last.dev.date"]]) %>%
          mutate(R = ntile(Score, bins)) %>%
          group_by(ID, R) %>%
          summarise(R.low = min(Score)) %>%
          mutate(R.low = ifelse(R == min(R), 0, R.low),
                 R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>% # works for round 4
          full_join(all, by = "ID") %>%
          filter(Score >= R.low, Score <= R.high) %>%
          ungroup() %>% arrange(ticker, ID, ds) %>%
          select(-c(R.low, R.high)) %>%
          left_join(mods, by = c("ticker", "R", "ID", "DP.Method", "Period", "MA.Type")) %>%
          mutate(algoId = "20191007") %>%
          Simulate.Trading(., Process = "Production")
  
  today <- Action.Today(sim) 
  
  if(file.exists(paste0("./Data/Simulation/", ticker, ".rds")))
  {
    sim <- bind_rows(sim, readRDS(paste0("./Data/Simulation/", ticker, ".rds"))) %>%
            distinct()
  }
  
  saveRDS(sim, paste0("./Data/Simulation/", ticker, ".rds"))
  rm(ticker, d1, all, mods, bins, sim)
  return(today)

}
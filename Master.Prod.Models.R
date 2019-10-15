# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
source("./Functions/M01.Scoring.R")

Parms <- list(algoIds = c("20191007")
              , buyalgos = c("20191007")
              # , acctCode         = "DU1617055"   # IB Account Number (Paper)
              , acctCode         = "U3263834"   # IB Account Number (Live)
              
              , invest.max.model  = 1000   # Maximum investment in a model
              , invest.max.ticker = 3000   # Maximum investment in a ticker
              , max.capacity      = 0.01   # Max % of yesterday's volume any model can buy
              , data.folder       = "./Data/"
              , last.dev.date     = as.Date("2019-01-31")
              )

prod.models <- data.frame()
for (algoId in Parms$algoIds)
{
  prod.models <- bind_rows(prod.models, 
                           readRDS(paste0("./Data/Summary/", algoId, ".Production.Models.rds")))
  rm(algoId)
}

# Selecting Models in buyalgos or having a position
if(file.exists("./Data/Trading/00.Latest.rds"))
{
  in.hand <- readRDS("./Data/Trading/00.Latest.rds") %>% ungroup() %>%
              filter(account == Parms[["acctCode"]]) %>% select(-account)

  prod.models <- prod.models %>% arrange(ticker, ID, R, algoId) %>%
                  left_join(in.hand, by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
                  filter(algoId %in% Parms$buyalgos | !is.na(units)) %>%
                  select(-units)
  
  rm(in.hand)
}

# -------------------------------------------------------------------------
# Incremental Data Pull: Works in the middle of the trading day
# -------------------------------------------------------------------------
hist.d1 <- readRDS("./Data/Summary/Clean.Prices.rds") %>% 
            filter(ticker %in% unique(prod.models$ticker))

all.d1 <- Get.Incremental.Data(stocks = unique(hist.d1$ticker), first.date = max(hist.d1$ds))
rm(hist.d1)
# -------------------------------------------------------------------------
# Rescoring with New Data
# -------------------------------------------------------------------------
do.call(unlink, list(list.files(c("./Data/Process.Tracker/", "./Data/Scores/"), full.names = TRUE)))

stocks <- setdiff(unique(all.d1$ticker),
                  gsub(".rds", "", list.files("./Data/Process.Tracker/")))

foreach(ticker = stocks
        , .export = c("AF.LN", "AF.MALN"), .packages = c("dplyr", "foreach")
        , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove' ) %dopar%
        {
          # ticker <- "MDB"
          d1 <- all.d1 %>% filter(ticker == !!ticker)
          T.models <- prod.models %>% filter(ticker == !!ticker) %>%
                        select(DP.Method, MA.Type, Period, ID) %>% distinct()
            
          Get.Model.Scores(ticker, d1, T.models, Type = "Production")
          saveRDS(ticker, paste0("./Data/Process.Tracker/", ticker, ".rds"))
          
          rm(ticker, d1, T.models)
        }

rm(list = lsf.str())
rm(stocks)

# -------------------------------------------------------------------------
# Get Targets: Stocks in active zone + price points 
# Filters investment in more than 3 models
# Assigns the #units bought/sold
# -------------------------------------------------------------------------
source("./Functions/M02.Simulation.R")

do.call(unlink,
        list(list.files(c("./Data/Process.Tracker/", "./Data/Simulation/"), full.names = TRUE)))

stocks <- setdiff(gsub(".rds", "", list.files("./Data/Scores/")), 
                  gsub(".rds", "", list.files("./Data/Process.Tracker/")))

targets <- foreach(ticker = stocks, .combine = bind_rows, .packages = c("dplyr", "foreach"),
                   .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
                   ) %dopar%
            {
              targets <- Prod.20191007(ticker)
              saveRDS(ticker, paste0("./Data/Process.Tracker/", ticker, ".rds"))
              return(targets)
            }

saveRDS(targets, paste0("./Data/Trading/01.Targets.rds"))
View(targets)
# -------------------------------------------------------------------------
do.call(unlink, list(list.files(c("./Data/Process.Tracker/"), full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()

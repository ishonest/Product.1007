Parms <- list(algoIds = c("20191007")
              , buyalgos = c("20191007")
              # , acctCode         = "DU1617055"   # IB Account Number (Paper)
              , acctCode         = "U3263834"   # IB Account Number (Live)
              
              , invest.max.model  = 1000   # Maximum investment in a model
              , invest.max.ticker = 3000   # Maximum investment in a ticker
              , max.capacity      = 0.01   # Max % of yesterday's volume any model can buy
              , data.folder       = "./Data/"
              , last.dev.date     = as.Date("2019-01-31")
              
              , clientId         = 100           # Must be a positive integer
              , Emergency        = FALSE
              , System.Live      = FALSE
              , Last.Order.Time  = Sys.time()
              
              , invest.max       = 10911         # Maximum position
              , invest.min       = 500           # Minimum investment in a model
              , Start.Trading.At = 9.35          # NY Time to start buying
              , Stop.Trading.At  = 16            # NY Time to stop trading
              , Last.Sell.At     = 15.50         # NY Time to initiate EOD Sell
              , Start.Plot.From  = as.Date("2019-01-01")
              )

saveRDS(Parms, "./Data/Summary/System.Parameters.rds")
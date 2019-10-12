# WARNING: Start TWS Before Proceeding
# -------------------------------------------------------------------------
# Global Parameter Calls
# -------------------------------------------------------------------------
IB.Parms <- list(acctCode         = "U3263834"   # IB Account Number (Live)
                 # acctCode         = "DU1617055"   # IB Account Number (Paper)
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

# -------------------------------------------------------------------------
source("./Functions/T01.Start.R")

while(lubridate::hour(format(Sys.time(), tz = "US/Eastern")) < IB.Parms[["Stop.Trading.At"]])
{
  View(IB.01.targets)
  IB.Account.Status()
  IB.System.Status()
  
  if(IB.Parms[["System.Live"]])
  {
    IB.Actions()
    View(IB.02.actions)    
    IB.Order()
    IB.Action.Plots()
  }
  
  IB.Next.Run(wait.seconds = 120)
  
  if(IB.Parms[["System.Live"]])
  {
    IB.Cancel.Orders()
    Update.Activity()
    View(IB.03.orders)
    View(IB.04.activity)
  }
}

# -------------------------------------------------------------------------
# End of Day Process
# -------------------------------------------------------------------------
source("./Functions/T06.ShutterDown.R")

# -------------------------------------------------------------------------
# WARNING: Emergency Process: All positions will be closed
# -------------------------------------------------------------------------
# # source("./Functions/T07.Emergency.R")
# -------------------------------------------------------------------------
# WARNING: Restarting System: All logs will be deleted
# Do Not Use Unless Necessary
# -------------------------------------------------------------------------
# # source("./Functions/T00.Restart.R")
# -------------------------------------------------------------------------


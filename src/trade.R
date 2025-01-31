# trade_buy <- function(accnt, curr, price){
#   if(price >= curr$low & price <= curr$high){
#     lots <- floor(accnt / (100 * price))
#   }else{
#     lots <- 0
#   }
#   
#   # cache result
#   # ...
#   
#   return(lots)
# }
# 
# trade_sell <- function(hold, curr, price){
#   if(price >= curr$low & price <= curr$high){
#     lots <- hold
#   }else{
#     lots <- 0
#   }
#   
#   # cache result
#   # ...
#   
#   return(lots)
# } 


trade_lots <- function(dire, curr, price, accnt, hold){
  if(dire == "buy"){
    if(price >= curr$low & price <= curr$high){
      lots <- floor(accnt / (100 * price))
    }else{
      lots <- 0
    }
  }

  if(dire == "sell"){
    if(price >= curr$low & price <= curr$high){
      lots <- -hold
    }else{
      lots <- 0
    }
  }
  
  return(lots)
}

trade_record <- function(dire, lots, price, curr){
  data.frame(
    trade_date = curr$trade_date,
    trade_dire = dire,
    trade_lots = lots,
    trade_price = price
  )
}

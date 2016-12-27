# based on https://mapzen.com/documentation/overview/#rate-limits
# limits for keyless access are 1/second and 6/minute
# see issue #4
mapzen_elev_GET_nokey <- ratelimitr::limit_rate(
  httr::GET, 
  ratelimitr::rate(n = 1, period = 1), ratelimitr::rate(n = 5, period = 75))

# based on https://mapzen.com/documentation/overview/#rate-limits
# limits for valid keyholders are 2/second and 20K/day
# this function will only enforce the 2/second limit
# see issue #4
mapzen_elev_GET_withkey <- ratelimitr::limit_rate(
  httr::GET,
  ratelimitr::rate(n = 2, period = 1)
)

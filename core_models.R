
# SIMULATING A TENNIS MATCH

# What variables do we need?

# - probability of each player winning a service point
# --> prb of successful first serve
#       * prb of ace
#       * prb of service winner
#       * prb of win post-rally
#       * prb of lose post-rally
# --> prb of successful second serve
#       * prb of ace
#       * prb of service winner
#       * prb of win post-rally
#       * prb of lose post-rally
# --> else: double fault

# ___________________________________________________________________

# FN game ===========================================================

# probability of winning a game
# as a fn of server's point-winning probability
prb.game <- function(SPP) {
  if (!is.numeric(SPP) | !(SPP>=0 & SPP<=1)) message("Bad val: SPP")
  OPP <- 1 - SPP
  prb <- numeric(4)
  # 40-0, only one way this can happen
  prb[1] <- SPP^4
  # 40-15, four ways this can happen
  prb[2] <- SPP^4 * OPP * 4
  # 40-30, ten ways this can happen (5c2)
  prb[3] <- SPP^4 * OPP^2 * 10
  # 40-40, twenty ways this can happen (6c3)
  prb[4] <- SPP^3 * OPP^3 * 20
  # multiply last by prb of winning deuce
  prb[4] <- prb[4] * (SPP^2 / (SPP^2 + OPP^2))
  # name + return vector
  names(prb) <- c("40-0", "40-15", "40-30", "40-40")
  prb
}
prb.game <- Vectorize(prb.game)
# ^ vector of prbs for four different outcomes

# ___________________________________________________________________

p.game <- function(SPP) {colSums(prb.game(SPP))}
# ^ single number for win probability

# ___________________________________________________________________

# FN tiebreak =======================================================

# the following function returns a data.frame
# that tabulates the possble tiebreak outcomes
# resulting in a win by p1 -- this will be 
# used in conjunction with the point-winning
# probabilities in a later function
gen_TB <- function() {
  
  tb_order <- c(1L,0L,0L,1L,1L,0L,0L,1L,1L,0L,0L,1L)
  # ^ 1 = service point, 0 = return point
  
  # possible winning outcomes for p1
  tb_df <- data.frame(
    p1p = c(7L,7L,7L,7L,7L,7L,6L), # p1 points
    p2p = c(0L,1L,2L,3L,4L,5L,6L)  # p2 points
  )
  
  # tp := total points
  tb_df$tp <- as.integer(rowSums(tb_df))
  
  # tsp := total service points
  tb_df$tsp <- sapply(tb_df$tp, function(p) {
    sum(tb_order[1:p])
  })
  
  # trp := total return points
  tb_df$trp <- tb_df$tp - tb_df$tsp
  
  # indicators for whether last points is serve/return
  tb_df$lps <- tb_order[tb_df$tp]
  tb_df$lpr <- 1L - tb_order[tb_df$tp]
  # set to zero if final score is 6-6
  tb_df$lps[tb_df$p2p == 6L] <- 0L
  tb_df$lpr[tb_df$p2p == 6L] <- 0L
  
  # for each row of tb_df, we want to know
  # all of the different ways the points could be
  # split in terms of service/return points won
  # by p1 and p2
  tb_cases <- lapply(1:7, function(i) {
    r <- tb_df[i,]
    row.names(r) <- NULL
    p1_max_sp <- r$tsp # maximum service points for p1
    p1_min_sp <- r$p1p - r$trp # min service pts for p1
    tmp <- data.frame(p1sp = p1_max_sp:p1_min_sp)
    cbind(r,tmp)
  })
  
  tb_df <- do.call(rbind, tb_cases)
  
  # p1 return pts; p2 service and return pts
  tb_df$p1rp <- tb_df$p1p - tb_df$p1sp
  tb_df$p2sp <- tb_df$tsp - tb_df$p1sp
  tb_df$p2rp <- tb_df$trp - tb_df$p1rp
  
  # to calculate the overall probability of each
  # state, e.g. winning 7-3, we need the number of 
  # combinations of wins that results in that state
  
  # will do this from the POV of p2, but could
  # do it from p1's POV and get equivalent results
  
  # because p2 can't win the last serve if the score
  # is 7-x, we will subtract one point from the 
  # relevant choice set
  tb_df$c_sp <- choose(tb_df$tsp - tb_df$lps, tb_df$p2sp)
  tb_df$c_rp <- choose(tb_df$trp - tb_df$lpr, tb_df$p2rp)
  tb_df$comb <- tb_df$c_sp * tb_df$c_rp
  
  # sum(tb_df$comb) # CHECK: should be 1716
  
  tb_df
  
}

TB <- gen_TB()

# ___________________________________________________________________

# probability of winning a tiebreak
# as a fn of player 1's point-winning probabilities
# SPP := service point probability
# RPP := return point probability
prb.tiebreak <- function(SPP, RPP, X = TB) {
  
  OSPP <- 1 - SPP
  ORPP <- 1 - RPP
  
  tmp <- data.frame(
    SPP = SPP, RPP = RPP,
    OSPP = OSPP, ORPP = ORPP
  )
  
  # add probabilities to table of outcomes
  X <- cbind(tmp, X)
  
  # for each row in the table, compute likelihood
  X$L <- with(X,
    SPP^p1sp * RPP^p1rp * OSPP^p2sp * ORPP^p2rp * comb)
  
  # sum likelihood within each category (7-0, 7-1, etc.)
  X <- aggregate(list(L = X$L), 
                 list(p1p = X$p1p, p2p = X$p2p),
                 sum)
  
  out <- X$L
  
  # need to multiply 6-6 scenario by appropriate factor
  out[7] <- out[7] * 
    (SPP*RPP) / (SPP*RPP + (1-SPP)*(1-RPP))
  
  # rename and return
  names(out) <- paste0(X$p1p,"-",X$p2p)
  out
  
}
prb.tiebreak <- Vectorize(prb.tiebreak)

# ___________________________________________________________________

p.tiebreak <- function(SPP, RPP, TB = TB) {
  colSums(prb.tiebreak(SPP, RPP))
}

# ___________________________________________________________________

# FN set ============================================================

# the following function returns a data.frame
# that tabulates the possble set outcomes
# resulting in a win by p1 -- this will be 
# used in conjunction with the game-winning
# probabilities in a later function
gen_ST <- function() {
  
  st_order <- rep(1:0,6)
  # ^ 1 = service game, 0 = return game
  
  # possible winning outcomes for p1
  st_df <- data.frame(
    p1g = c(6L,6L,6L,6L,6L,7L,6L), # p1 games
    p2g = c(0L,1L,2L,3L,4L,5L,6L)  # p2 games
  )
  
  # tp := total games
  st_df$tg <- as.integer(rowSums(st_df))
  
  # tsp := total service games
  st_df$tsg <- sapply(st_df$tg, function(p) {
    sum(st_order[1:p])
  })
  
  # trp := total return games
  st_df$trg <- st_df$tg - st_df$tsg
  
  # indicators for whether last game is serve/return
  st_df$lgs <- st_order[st_df$tg]
  st_df$lgr <- 1L - st_order[st_df$tg]
  # set to zero if final score is 6-6
  # st_df$lgs[st_df$p2g == 6L] <- 0L
  # st_df$lgr[st_df$p2g == 6L] <- 0L
  
  # for each row of st_df, we want to know
  # all of the different ways the games could be
  # split in terms of service/return games won
  # by p1 and p2
  st_cases <- lapply(1:7, function(i) {
    r <- st_df[i,]
    row.names(r) <- NULL
    p1_max_sg <- r$tsg # maximum service games for p1
    p1_min_sg <- r$p1g - r$trg # min service games for p1
    tmp <- data.frame(p1sg = p1_max_sg:p1_min_sg)
    cbind(r,tmp)
  })
  
  st_df <- do.call(rbind, st_cases)
  
  # p1 return games; p2 service and return games
  st_df$p1rg <- st_df$p1g - st_df$p1sg
  st_df$p2sg <- st_df$tsg - st_df$p1sg
  st_df$p2rg <- st_df$trg - st_df$p1rg
  
  # indicator for 12-game set
  st_df$g12 <- as.integer(st_df$tg == 12)
  
  # to calculate the overall probability of each
  # state, e.g. winning 6-3, we need the number of 
  # combinations of wins that results in that state
  
  # will do this from the POV of p2, but could
  # do it from p1's POV and get equivalent results
  
  # because p2 can't win the last serve if the score
  # isn't 6-6, we will subtract one game from the 
  # relevant choice set
  st_df$c_sg <- with(st_df, choose(tsg - lgs - g12, p2sg))
  st_df$c_rg <- with(st_df, choose(trg - lgr, p2rg))
  st_df$comb <- pmax(st_df$c_sg * st_df$c_rg, 1)
  # special case for 6-6
  st_df$comb[st_df$p2g == 6L] <- with(
    st_df[st_df$p2g == 6L,],
    choose(tsg-1,p2sg-1) * choose(trg-1,p2rg) +
    choose(tsg-1,p2sg) * choose(trg-1,p2rg-1)
  )
  
  sum(st_df$comb) # CHECK: should be _____
  
  st_df
  
}

ST <- gen_ST()

# ___________________________________________________________________

# probability of winning a set 
# as a fn of player 1's point-winning probabilities
# sgp := service game-winning probability
# rgp := return game-winning probability
# also uses p.tiebreak()
prb.set <- function(SPP, RPP, X = ST,
                    has_tiebreak = TRUE) {
  # browser()
  SGP <- p.game(SPP)
  OSGP <- 1 - SGP
  ORGP <- p.game(1 - RPP)
  RGP <- 1 - ORGP
  
  tmp <- data.frame(
    SGP = SGP, RGP = RGP,
    OSGP = OSGP, ORGP = ORGP
  )
  
  # add probabilities to table of outcomes
  X <- cbind(tmp, X)
  
  # for each row in the table, compute likelihood
  X$L <- with(X,
    SGP^p1sg * RGP^p1rg * OSGP^p2sg * ORGP^p2rg * comb)
  
  # sum likelihood within each category (7-0, 7-1, etc.)
  X <- aggregate(list(L = X$L), 
                 list(p1g = X$p1g, p2g = X$p2g),
                 sum)
  
  out <- X$L
  
  # need to multiply 6-6 scenario by appropriate factor
  if (has_tiebreak) {
    out[7] <- out[7] * p.tiebreak(SPP, RPP)
  } else {
    out[7] <- out[7] * 
      (SGP*RGP) / (SGP*RGP + (1-SGP)*(1-RGP))
  }
  
  # rename and return
  names(out) <- paste0(X$p1g,"-",X$p2g)
  out
  
}
prb.set <- Vectorize(prb.set)

# ___________________________________________________________________

p.set <- function(SPP, RPP, ST = ST,
                  has_tb = TRUE) {
  colSums(prb.set(SPP, RPP, has_tiebreak = has_tb))
}

# ___________________________________________________________________

# FN match ==========================================================

prb.match <- function(SPP, RPP, 
                      best_of = 3L,
                      final_tiebreak = TRUE) {
  
  p <- p.set(SPP, RPP)
  p_notb <- p.set(SPP, RPP, has_tb = FALSE)
  prb <- numeric(0)
  
  if (best_of == 3L) {
    prb[1] <- p^2                  # 2-0
    prb[2] <- p^2 * (1-p) * 2      # 3-2
    names(prb) <- c("2-0", "3-2")
  } else if (best_of == 5L) {
    prb[1] <- p^3                         # 3-0
    prb[2] <- p^3 * (1-p) * 3             # 3-1
    prb[3] <- p^2 * p_notb * (1-p)^2 * 6  # 3-2
    names(prb) <- c("3-0", "3-1", "3-2")
  }
  prb
}
prb.match <- Vectorize(prb.match)

# ___________________________________________________________________

p.match <- function(SPP, RPP, 
                    best_of = 3L,
                    final_tiebreak = TRUE) {
  colSums(prb.match(SPP, RPP, best_of, final_tiebreak))
}

# ___________________________________________________________________



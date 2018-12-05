## Code to generate tricky data.


#' Sample incomplete rank data where the first half come from one
#' group and the second half come from the other group. The only
#' difference between the groups is that one has 1 before 2 all the time
#' and the other has 2 before 1 all the time.
#'
#' @param nobj The number of objects to rank.
#' @param nind The number of individuals who rank.
#' @param pmiss The proportion of missing elements per ranking
#'
#' @return A matrix where each row is a ranking
#'
#' @author David Gerard
gen_12diff <- function(nobj, nind, pmiss) {
  stopifnot((nind %% 2) == 0) ## check even
  stopifnot(nind > 0, nobj > 0, pmiss > 0, pmiss <= 1)
  dat <- matrix(NA, nrow = nind, ncol = nobj)

  ## Sim group 1
  for(index in 1:(nind / 2)) {
    order_temp <- sample(1:nobj)
    dat[index, ] <- c(sort(order_temp[1:2], decreasing = FALSE),
                      order_temp[3:nobj])
  }

  ## Sim group 2
  for(index in 1:(nind / 2)) {
    order_temp <- sample(1:nobj)
    dat[index + nind/2, ] <- c(sort(order_temp[1:2], decreasing = TRUE),
                               order_temp[3:nobj])
  }

  numsamp <- nind * nobj * pmiss
  dat[sample(1:(nind * nobj), size = numsamp)] <- NA
  return(dat)
}

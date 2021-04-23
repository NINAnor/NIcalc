#' Weights per Indicator, trophic Group, BSunit or ICunit.
#'
#' Summary method for objects of class \code{\link{niSeries}}.
#'
#' @seealso \code{\link{calculateIndex}}, \code{\link{niSeries}}, and
#' \code{\link{niOutput}} for cumputation and generation of \code{niSeries} and
#' \code{niOutput} objects.
#'
#' @name summaryWeights
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param x an \code{niSeries} object.
#' @param group character. Should summary be per indicator (\code{group = "indic"}, default), per trophic group
#' (\code{group = "troph"}), per BSunit (\code{group = "BSunit"}), or per ICunit (\code{group = "ICunit"})?
#' @param ... argument controlling the summary: \code{indicData} logical flag, whether to include additional
#' variables characterizing indicators (i.e. trophic group, fidelity) in the summary per indicator or not,
#' default is \code{F}.
#'
#' @return a numeric matrix (when \code{group in c("troph","BSunit") or indicData = F})
#' or a dataframe (when \code{group = "ICunit" or indicData = T}) with weights
#' per indicator (\code{group = "indic"}),
#' per trophic group (\code{group = "troph"}),
#' per BSunit (\code{group = "BSunit"}), or
#' per ICunit (\code{group = "ICunit"}),
#' for each year included in \code{x}.
#'
#' @examples
#' \dontrun{
#' indices <- calculateIndex(x = themeData,
#'                        nsim = 1000,
#'                        fids = FALSE,
#'                        tgroups = FALSE,
#'                        keys = "ignore",
#'                        w = 0,
#'                        awbs = TRUE,
#'                        awBSunit = "Skog")
#' summaryWeights(indices$wholeArea, indicData = TRUE)
#' summaryWeights(indices$E, group = "ICunit")
#' summaryWeights(indices$E, group = "BSunit")
#' }
#'
#' @export
#'

summaryWeights <- function(x = NULL, group = "indic", ...) {

  z <- match.call()

  if (!is.niSeries(x)) {
    stop(paste("'",z[2],"' is not of class 'niSeries'.", sep=""), call. = TRUE)
  }

  validSummaries <- c("indic","troph","BSunit","ICunit")

  if (!(group %in% validSummaries)) {
    stop(paste("'",group,"' is not a summary type.\nValid types are '",
               paste(validSummaries,collapse = "', '"),"'.", sep=""), call. = TRUE)
  }

  setSummaryParameters <- function(indicData = F) {

    return(list(indicData = indicData))
  }

  param <- setSummaryParameters(...)
  indicData <- param$indicData

  yearNames <- names(x)
  nYears <- length(yearNames)
  years <- as.integer(yearNames)

  if (group %in% c("indic","troph")) {
    indics <- NULL
    for (j in 1:nYears) {
      indics<- c(indics,x[[j]]$indicators)
    }
    indics <- sort(unique(indics))
    nindics <- length(indics)
    y <- matrix(NA,nrow=nindics,ncol=nYears)
    dimnames(y) <- list(indics,yearNames)
    for (j in 1:nYears) {
      if(is.array(x[[j]]$indexWeights)) {
        yy <- colSums(x[[j]]$indexWeights,na.rm = TRUE)
      } else {
        yy <- x[[j]]$indexWeights
      }
      y[names(yy),j] <- yy
    }
    dimnames(y)[[2]] <- paste(x[[j]]$indexArea,yearNames)

    if (group == "troph") {indicData <- TRUE}

    if (indicData) {
      if (nYears == 1) {
        indicatorData <- x[[1]]$indicatorData
      } else {
        indicatorData <- x[[1]]$indicatorData
        for (j in 2:nYears) {
          indicatorData <- merge(indicatorData,x[[j]]$indicatorData,all=T)

        }
      }

      matchNames <- c("id", "name","keyElement","functionalGroup","functionalGroupId","scalingModel","scalingModelId")
      dataFound <- which(match(names(indicatorData), table = matchNames, nomatch = 0) == 0)
      matchNames <- c("name","keyElement","functionalGroup")
      dataFound2 <- which(match(names(indicatorData), table = matchNames, nomatch = 0) != 0)
      indicatorData <- indicatorData[,c(dataFound2,dataFound)]

      y <- cbind(indicatorData[match(dimnames(y)[[1]], table = indicatorData$name, nomatch = 0),],y)
      dimnames(y)[[1]] <- y$name

      y <- y[,dimnames(y)[[2]][-which(dimnames(y)[[2]] == "name")]]
    }

    if (group == "troph") {
      newGroup <- ifelse(y$keyElement,"Key indicator",y$functionalGroup)
      ytg <- matrix(NA,nrow = length(unique(newGroup)),ncol = nYears)
      dimnames(ytg) <- list(unique(newGroup),dimnames(y)[[2]][4:(3+nYears)])
      for (j in 1:nYears) {
        ytg[,j] <- tapply(y[,j+3],as.factor(newGroup),sum,na.rm=TRUE)[dimnames(ytg)[[1]]]
      }
      y <- ytg
    }

    return(y)
  }

  if (group == "BSunit") {
    y <- NULL
    for (j in 1:nYears) {
      y <- cbind(y,x[[j]]$NIunitWeights)
    }
    dimnames(y)[[2]] <- paste(x[[j]]$indexArea,yearNames)
    return(y)
  }

  if (group == "ICunit") {
    y <- yyy <- indics <- NULL
    for (j in  yearNames) {
      k <- 0
      yy <- ind <- NULL
      for (i in x[[j]]$ICunits) {
        k <- k + 1
        yy[k] <- sum(x[[j]]$indexWeights[x[[j]]$ICunitMatrix == i],na.rm = TRUE)
        ind[k] <- names(which(colSums(x[[j]]$ICunitMatrix == i,na.rm = TRUE) > 0))
      }
      names(yy) <- x[[j]]$ICunits
      yyy[[j]] <- yy
      indics[[j]] <- ind
      y <- c(y,names(yy))
    }

    y <- unique(y)

    yy <- ind <- matrix(NA,nrow=length(y),ncol=nYears)
    dimnames(yy) <- dimnames(ind) <-list(y,yearNames)
    for (j in  yearNames) {
      yy[names(yyy[[j]]),j] <- yyy[[j]]
      ind[names(yyy[[j]]),] <- indics[[j]]
    }

    indics <- ind[,1]
    y <- data.frame(ind[,1],yy)
    dimnames(y)[[2]] <- c("indicator",paste(x[[j]]$indexArea,yearNames))
    y <- y[order(y$indicator,dimnames(y)[[1]]),]
    return(y)
  }

}

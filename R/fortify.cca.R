##' @title Fortify a \code{"cca"} object.
##'
##' @description
##' Fortifies an object of class \code{"cca"} to produce a
##' data frame of the selected axis scores in long format, suitable for
##' plotting with \code{\link[ggplot2]{ggplot}}.
##'
##' @details
##' TODO
##'
##' @param model an object of class \code{"cca"}, the result of a call to
##' \code{\link[vegan]{cca}}, \code{\link[vegan]{rda}}, or
##' \code{\link[vegan]{capscale}}.
##' @param data currently ignored.
##' @param ... additional arguments passed to \code{\link[vegan]{scores.cca}}.
##' @return A data frame in long format containing the ordination scores.
##' The first two components are the axis scores.
##' @author Gavin L. Simpson
##'
##' @export
##' @method fortify cca
##'
##' @importFrom ggplot2 fortify
##'
##' @examples
##' data(dune)
##' data(dune.env)
##'
##' sol <- cca(dune ~ A1 + Management, data = dune.env)
##' head(fortify(sol))
`fortify.cca` <- function(model, data, alpha = 0.05, ...) {
    # added default alpha of 0.05 for statistical analysis
    scrLen <- function(x) {
        obs <- nrow(x)
        if(is.null(obs))
            obs <- 0
        obs
    }
    
    # Identify significant axes of variation
    model_axis_anova <- anova(model, by = "axis")
    sig_axis_count <- length(which(model_axis_anova$"Pr(>F)" < alpha))
    
    scrs <- scores(model, display = c("sp", "wa", "lc", "bp", "cn"), 
                    choices = 1:sig_axis_count,
                    ...) # added 'choices' to display the number of significant RDA axes for further plotting and interpretation
    rnam <- lapply(scrs, rownames)
    take <- !sapply(rnam, is.null)
    rnam <- unlist(rnam[take], use.names = FALSE)
    scrs <- scrs[take]
    fdf <- do.call(rbind, scrs)
    rownames(fdf) <- NULL
    fdf <- data.frame(fdf)
    lens <- sapply(scrs, scrLen)
    fdf$Score <- factor(rep(names(lens), times = lens))
    fdf$Label <- rnam
    attr(fdf, "dimlabels") <- names(fdf)[1:sig_axis_count] # extended the names to include all significant axes
    names(fdf)[1:sig_axis_count] <- paste0("Dim", 1:sig_axis_count) # extended the names to include all significant axes
    fdf
}


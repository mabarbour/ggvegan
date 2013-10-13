##' @title ggplot-based plot for objects of class \code{"cca"}
##'
##' @description
##' Produces a multi-layer ggplot object representing the output of
##' objects produced by \code{\link[vegan]{cca}}, \code{\link[vegan]{rda}},
##' or \code{\link[vegan]{capscale}}.
##'
##' @details
##' TODO
##'
##' @param object an object of class \code{"cca"}, the result of a call
##' to \code{\link[vegan]{cca}}.
##' @param geom character; which geoms to use for the layers. Can be a
##' vector of length equal to \code{length(display)}, in which case the
##' \emph{i}th element of \code{type} refers to the \emph{i}th element
##' of \code{display}.
##' @param layers character; which scores to plot as layers
##' @param xlab character; label for the x-axis
##' @param ylab character; label for the y-axis
##' @param const General scaling constant to \code{rda} scores. See
##' \code{\link[vegan]{plot.cca}} for details.
##' @param ... Additional arguments passed to \code{\link{fortify.cca}}.
##' @return Returns a ggplot object.
##' @author Gavin L. Simpson
##'
##' @export
##' @method autoplot cca
##'
##' @importFrom grid arrow unit
##' @importFrom ggplot2 autoplot
##'
##' @examples
##' data(dune)
##' data(dune.env)
##'
##' sol <- cca(dune ~ A1 + Management, data = dune.env)
##' autoplot(sol)

## help codetools "find" variables - i.e. ignore the warnings in the
## ggplot() calls below - doing this on per-function basis in case
## R goes that way in the future. As ?globalVariables states, this
## is really active at the package level
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("Dim1", "Dim2", "Score", "Label"))
}
## end help codetools


`autoplot.cca` <- function(object, geom = c("point","text"),
                           layers = c("species", "sites", "biplot", "centroids"),
                           ylab, xlab, const, ...) {
    obj <- fortify(object, ...)
    LAYERS <- levels(obj$Score)
    dimlabels <- attr(obj, "dimlabels")
    ## match the geom
    geom <- match.arg(geom)
    point <- TRUE
    if (isTRUE(all.equal(geom, "text")))
        point <- FALSE
    ## subset out the layers wanted
    ### need something here first to match acceptable ones?
    ### or just check that the layers selected would return a df with
    ### at least 1 row.
    obj <- obj[obj$Score %in% layers, , drop = FALSE]
    ## skeleton layer
    plt <- ggplot()
    ## add plot layers as required
    want <- obj$Score %in% c("species", "sites")
    if (point) {
        plt <- plt +
            geom_point(data = obj[want, , drop = FALSE ],
                       aes(x = Dim1, y = Dim2, shape = Score,
                           colour = Score))
    } else {
        plt <- plt +
            geom_text(data = obj[want, , drop = FALSE ],
                      aes(x = Dim1, y = Dim2, label = Label,
                          colour = Score))
    }
    ## remove biplot arrows for centroids if present
    if(all(c("biplot","centroids") %in% LAYERS)) {
        want <- obj$Score == "biplot"
        tmp <- obj[want, ]
        obj <- obj[!want, ]
        bnam <- tmp[, "Label"]
        cnam <- obj[obj$Score == "centroids", "Label"]
        obj <- rbind(obj, tmp[!bnam %in% cnam, , drop = FALSE])
    }
    if(any(want <- obj$Score == "constraints")) {
        if (point) {
            plt <- plt + geom_point(data = obj[want, , drop = FALSE ],
                                    aes(x = Dim1, y = Dim2))
        } else {
            plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                                   aes(x = Dim1, y = Dim2,
                                       label = Label))
        }
    }
    if(any(want <- obj$Score == "biplot")) {
        if (length(layers) > 1) {
            mul <- vegan:::ordiArrowMul(obj[want, , drop = FALSE ])
            obj[want, c("Dim1","Dim2")] <- mul * obj[want, c("Dim1","Dim2")]
        }
        col <- "navy"
        plt <- plt +
            geom_segment(data = obj[want, , drop = FALSE ],
                         aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
                         arrow = arrow(length = unit(0.2, "cm")),
                         colour = col)
        obj[want, c("Dim1","Dim2")] <- 1.1 * obj[want, c("Dim1","Dim2")]
        plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                               aes(x = Dim1, y = Dim2, label = Label))
    }
    if(any(want <- obj$Score == "centroids")) {
        plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                               aes(x = Dim1, y = Dim2, label = Label),
                               colour = "navy")
    }
    if(missing(xlab))
        xlab <- dimlabels[1]
    if(missing(ylab))
        ylab <- dimlabels[2]
    plt <- plt + xlab(xlab) + ylab(ylab)
    ## add equal scaling
    plt <- plt + coord_fixed()
    plt
}

### My idea is to create a customizable plot that highlights the significant environmental variables and species of interest from an rda() analysis.  
# focal_environment
`autoplot.focused.cca` <- function(object, anova_environment, focal_species, alpha = 0.05){
    obj <- fortify(object)
    anova_env <- fortify(anova_environment)
    focal_environment <- rownames(anova_env)[which(anova_env$"Pr(>F)" < alpha)]
    #LAYERS <- levels(obj$Score)
    dimlabels <- attr(obj, "dimlabels")
    #obj <- obj[obj$Score %in% layers, , drop = FALSE]
    ## skeleton layer
    plt <- ggplot()
    
     #species_scores <- subset(obj, Score == "species"
     
    focal_species_plot_info <- geom_segment(data = obj[which(obj$Label %in% focal_species), ],
                                                aes(x=0, y=0, xend=Dim1, yend=Dim2),
                                                arrow = arrow(length = unit(0.2, "cm")), colour = "black", linetype = "dashed")
    focal_species_text <- geom_text(data = obj[which(obj$Label %in% focal_species), ], 
                                                aes(x=Dim1*1.1, y=Dim2*1.1), label = focal_species)
    nonFocal_species_plot_info <- geom_segment(data = subset(obj, !(Label %in% focal_species) & Score == "species"), 
                                                aes(x=0, y = 0, xend = Dim1, yend = Dim2),
                                                arrow = arrow(length = unit(0.2, "cm")), colour = "grey", linetype = "dashed")
    focal_environment_plot_info <- geom_segment(data = obj[which(obj$Label %in% focal_environment), ], 
                                                aes(x = 0, y = 0, xend=Dim1, yend=Dim2), 
                                                arrow = arrow(length = unit(0.2, "cm")),colour = "black")
    focal_environment_text <- geom_text(data = obj[which(obj$Label %in% focal_environment), ],
                                                aes(x=Dim1*1.1, y=Dim2*1.1), label = focal_environment)
    nonFocal_environment_plot_info <- geom_segment(data = subset(obj, !(Label %in% focal_environment) & Score == "biplot"), 
                                                aes(x=0, y=0, xend=Dim1, yend=Dim2), 
                                                arrow = arrow(length = unit(0.2, "cm")), colour="grey")
    site_plot_info <- geom_text(data = subset(obj, Score == "sites"), 
                                                aes(x=Dim1, y = Dim2, label = Label), colour="black")
    
    # final plot
    plt + focal_species_plot_info + focal_species_text + nonFocal_species_plot_info + focal_environment_plot_info + 
        focal_environment_text + nonFocal_environment_plot_info + site_plot_info + xlab(dimlabels[1]) + ylab(dimlabels[2]) +
        coord_fixed() + theme_bw()
    }
    
    #mul <- vegan:::ordiArrowMul(obj[want, , drop = FALSE ])
            obj[want, c("Dim1","Dim2")] <- mul * obj[want, c("Dim1","Dim2")]

   

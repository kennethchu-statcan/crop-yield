
initializePlot <- function(
    textsize.title = 30,
    textsize.axis  = 20
    ) {

    require(ggplot2);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.ggplot <- ggplot(data = NULL) + theme_bw();
    output.ggplot <- output.ggplot + theme(
        title            = element_text(size = textsize.title, face = "bold"),
        legend.text      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.x      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.y      = element_text(size = textsize.axis,  face = "bold"),
        #axis.title.x     = element_blank(),
        #axis.title.y     = element_blank(),
        panel.grid.major = element_line(colour="gray", linetype=2, size=0.25),
        panel.grid.minor = element_line(colour="gray", linetype=2, size=0.25),
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(0.75,"in")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( output.ggplot );

    }

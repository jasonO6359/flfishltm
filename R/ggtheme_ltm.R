#Define LTM theme function for ggplot figures

#' Custom LTM theme for ggplot
#'
#' @return theme for ggplot figures
#' @export
#'
#' @examples
#' data(newnans)
#' ggplot2::ggplot(data=newnans, ggplot2::aes(x=TotalLength,y=TotalWeight)) + 
#' ggplot2::geom_point() +
#' theme_ltm()
theme_ltm <- function() {
  create_theme_ltm <- function(){ 
    
    font <- "sans"   #assign font family up front
    
    axis_col <- "#868686"
    
    theme_classic() %+replace%    #replace elements we want to change
      
      theme(
        
        #grid elements
        #panel.grid.major = element_blank(),    #strip major gridlines
        #panel.grid.minor = element_blank(),    #strip minor gridlines
        #axis.ticks = element_blank(),          #strip axis ticks
        
       #text elements
        plot.title = element_text(             #title
          family = font,            #set font family
          size = 12,                #set font size
          face = 'bold',            #"bold" for typeface
          hjust = 0.5,                #center align
          vjust = 0),               #change to raise or lower
        
        plot.subtitle = element_text(          #subtitle
          family = font,            #font family
          size = 10),               #font size
        
        plot.caption = element_text(           #caption
          family = font,            #font family
          size = 9,                 #font size
          hjust = 1),               #right align
        
        axis.line = element_line(color = axis_col),
        
        axis.title = element_text(             #axis titles
          family = font,            #font family
          size = 10,
          color = axis_col),               #font size
        
        axis.text = element_text(              #axis text
          family = font,            #axis famuly
          size = 9,
          color = axis_col),                #font size
        
        axis.ticks = element_line(color = axis_col),
        
        # axis.text.x = element_text(            #margin for axis text
        #   margin=margin(5)),
        
        legend.title = element_blank(),
        
        legend.position = "bottom",
        
        legend.text = element_text(color = axis_col),
        
        legend.box.margin = margin(-10,0,0,0),
        
        legend.margin = margin(0,0,0,0),
        
        strip.background = element_rect(fill = NA,
                                        color = axis_col),
        
        strip.text = element_text(color = "black")
        
      )
  }
  create_theme_ltm()
}

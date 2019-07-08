#' Rotate the x of y axis labelds of a ggplot graph
#' 
#' @export


rotate_axis_labels <- function(axis, angle, hjust) {
  
  
  if(axis == "x" | axis == "X"){
  
  theme <- theme(axis.text.x = element_text(angle = angle, hjust = 1))
  
  }
  
  
  if(axis == "y" | axis == "y"){
    
  theme <- theme(axis.text.y = element_text(angle = angle, hjust = 1))
    
  }

  return(theme)
  
    
}

#' @title filtering data
#' @param df  data frame
#' @param SPECIES  condition for filter
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 ggplot geom_point aes_string geom_smooth labs
#' @importFrom utils write.csv
#' @return  filtered data frame
#' @export
#'
#' @examples
#' \dontrun{myddt(ddt,SPECIES = "CCATFISH")}
myddt <- function(df, SPECIES){
  RIVER <-  WEIGHT <- LENGTH <- NULL
  newdf <- df %>% filter(SPECIES == {{SPECIES}})                 # data frame for only a specific SPECIES
  g <- ggplot(newdf, aes_string(x = "LENGTH",y = "WEIGHT")) +    # plot LENGTH Vs WEIGHT
    geom_point(aes_string(color = "RIVER" )) +                   # data points are colored according to the RIVER variable
    geom_smooth(formula = y~x +I(x^2), method = "lm")+           # a quadratic curve
    labs(title="Ningyuan Weng")                                  # my name appear on the title
  print(g)                                                       # print the plot

  list(head(df),head(newdf),(table(df$RIVER)/length(df$RIVER)))
  if("CCATFISH" %in% newdf$SPECIES){                             #save the data frame to the working directory as a csv file called LvsWforCCATFISH.csv
    write.csv(newdf,'LvsWforCCATFISH.csv')
  }
  if("SMBUFFALO" %in% newdf$SPECIES){                            #save the data frame to the working directory as a csv file called LvsWforSMBUFFALO.csv
    write.csv(newdf,'LvsWforSMBUFFALO.csv')
  }

}

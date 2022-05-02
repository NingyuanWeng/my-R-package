#' @title filtering data
#' @param df  data frame
#' @param cond  condition for filter
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 ggplot geom_point aes_string geom_smooth labs
#' @importFrom utils write.csv
#' @return  filtered data frame
#' @export
#'
#' @examples
#' \dontrun{myddt(ddt,SPECIES == "CCATFISH")}
myddt <- function(df, cond){

  df1 <- df %>% filter({{cond}}) # Note the use of {{}}
  g <- ggplot(df1, aes_string(x = "LENGTH",y = "WEIGHT")) + # Note the use of aes_string
    geom_point(aes_string(color = "RIVER" )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm")+
    labs(title="Ningyuan Weng")
  print(g)

  if("CCATFISH" %in% df1$SPECIES){
    write.csv(df1,'CCATFISH.csv')
  }
  if("SMBUFFALO" %in% df1$SPECIES){
    write.csv(df1,'SMBUFFALO.csv')
  }

}

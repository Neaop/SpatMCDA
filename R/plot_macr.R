#' @title Plot the Mean of Absolute Change Rates
#' @description Input the weights of all factors obtained by weight_fun() to obtain a weight plot.
#' @param data data.frame,a data frame that includes the rate of change of weights (%),MACRs (obtained from macr()) and factor names.
#'
#' @return A plot of the Mean of Absolute Change Rates
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 theme_bw
#' @examples
#' \donttest{
#' #Factor A
#' #change rate fo weight(%)
#' CRW <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
#' #Mean of absolute change rates (obtained by macr())
#' Value <- c(0.2273, 0.1819, 0.1364, 0.0909,0.0455,0,0.0455,0.0909,0.1364, 0.1819,0.2273)
#' #Factor name
#' Variable <- as.factor(rep("Factor_A",11))
#' macr_FactorA <- data.frame(CRW,Value,Variable)
#' #Factor B
#' #change rate fo weight(%)
#' CRW <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
#' #Mean of absolute change rates (obtained by macr())
#' Value <- c(0.2213,0.177,0.1328,0.0885,0.0443,0,0.0443,0.0885,0.1328,0.177,0.2213)
#' #Factor name
#' Variable <- as.factor(rep("Factor_B",11))
#' macr_FactorB <- data.frame(CRW,Value,Variable)
#' #Factor C
#' #change rate fo weight(%)
#' CRW <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
#' #Mean of absolute change rates (obtained by macr())
#' Value <- c(0.222,0.1776,0.1332,0.0888,0.0444,0,0.0444,0.0888,0.1332,0.1776,0.222)
#' #Factor name
#' Variable <- as.factor(rep("Factor_C",11))
#' macr_FactorC <- data.frame(CRW,Value,Variable)
#' #Merging the MACRs of all factors
#' MACR_Data <- rbind(macr_FactorA,macr_FactorB,macr_FactorC)
#' #Plot the Mean of Absolute Change Rates
#' plot_macr(MACR_Data)
#' }
plot_macr <- function(data){
  ggplot(data = data,aes(x=CRW,y=Value,color=Variable,shape=Variable))+
    geom_line(size=1)+
    ylab("Mean of absolute change rate of reasult(%)")+
    xlab("Change rate of weight(%)")+
    ggtitle("Sensitivity Analyis")+
    geom_point(size=2)+
    scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
    theme_bw()
}



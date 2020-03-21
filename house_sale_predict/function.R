GGvisualize_theme <- function(){
  require(ggplot2)
  theme_bw() + #去掉背景色
  theme(panel.grid=element_blank(),  #去掉網線
        panel.border=element_blank(),#去掉邊線
        axis.line=element_line(size=1,colour="black"))
}

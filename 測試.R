if(!require(ggplot2)) install.packages("ggpllot2")
if(!require(gapminder)) install.packages("gapminder")
if(!require(dplyr)) install.packages("dplyr")
###越富有的國家，iphone市占率越高
ggplot(rowdata,aes(x=income,y=market_share,size=gini,color=region))+
  geom_point()
###匯率波動越大，手機越貴，因此今年要先買IPOHE!?
I13 <- rowdata %>% filter(model_type=="13-256")
ggplot(I13,aes(x=vol_fx,y=price_index,color=region))+
  geom_point(size=5)
###在歐洲市場，收入越高的國家，iphone可能越貴
EU_I13 <- rowdata %>% filter(region=="Europe") %>%
  filter(model_type=="13-256") 
ggplot(EU_I13,aes(x=income,y=price_index,size=gdp_amt))+
  geom_point(color="red")
###手機越貴的地方，差別訂價越大
I13 <- rowdata %>% filter(model_type=="13-256")
ggplot(I13,aes(x=range_ratio,y=price_index,size=gdp_amt))+
  geom_point(color="blue")


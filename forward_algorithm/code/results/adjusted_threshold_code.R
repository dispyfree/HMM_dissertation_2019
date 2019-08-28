devs <- rep(ret$deviations[-1], each=20)

time <- 1:280

dat <- data.frame(x = time, deviations = devs / 0.3 , sdFac = head(ret$sdFacs, -1), convThreshold = head(ret$convLimits, -1) / 0.3 )
ggplot(data=dat, aes(x=time)) + geom_line(aes(y = sdFac, color='sdFac')) + geom_line(aes(y = convThreshold, color='convThreshold')) + geom_line(aes(y = deviations, color='deviation')) + geom_hline(yintercept= (0.05/0.3), colour='darkblue', linetype='dashed') + scale_colour_manual(  values=c("darkblue","darkred","darkgreen"))+ ggtitle('corrective factor and adjusted threshold') 



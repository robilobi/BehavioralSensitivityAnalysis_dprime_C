######
dat=read.table(file="dataexample_forDprime.csv",header=T, sep="\t")  # read data
source("FUNsensitivity.R") ## it needs function(response, signal, units = NULL, conditions = NULL, hitmax , famax )
response=dat$totresponse; signal=dat$signal; subj=dat$subj; block=dat$block;
df=dprime(response, signal, unit = subj, condition = block, 9, 18)  #use function* FUNsensitivity (dprime)
df1=melt(df, id.vars=c("unit", "condition"))
write.table(df, file=paste0(dir, "dataoutput.csv"), row.names=F, col.names=T, sep="\t")    #to compu
> map2 = function(k,flights) {
       return ( keyval(as.character(flights[[9]]),flights[[22]]))
}
> reduce2 = function(carrier, counts) {
     keyval(carrier, sum(counts))
}
>
> mr2 = function(input, output = NULL) {
     mapreduce(input = input,
               output = output,
               input.format = make.input.format("csv", sep=","),
               map = map2,
               reduce = reduce2)}
>
> hdfs.root = '/user/huang48'
> hdfs.data = file.path(hdfs.root,'2004.csv')
> hdfs.out = file.path(hdfs.root,'out2004JIE')
> out = mr2(hdfs.data, hdfs.out)
> results = from.dfs(out)
> results.df = as.data.frame(results, stringsAsFactors=F)
> # add column heading to dataframe
> colnames(results.df) = c('Carrier', 'Cancelled')
>
> # Display results
> results.df
   Carrier Cancelled
1       AA     12402
2       AS      3670
3       B6       511
4       CO      1898
5       DH      8287
6       DL     10717
7       EV      7542
8       FL      2013
9       HA       162
10      HP      3108
11      MQ     17787
12      NW      6010
13      OH     13616
14      OO      9784
15      TZ       717
16      UA      6548
17      US      6779
18      WN     10103
19      XE      6103
> 
library(data.table)

command<-"find /usr/share/wordnet/dict/ -type f -exec cat {} + | sed 's/ /\\n/g\' | grep -oE '^[[:lower:]]+' |grep -E '^[[:alpha:]]{5}$'  | sort | uniq "
system(command,intern=TRUE)->t
data.table(V0=t)->t
substr(t$V0,1,1)->t$V1
substr(t$V0,2,2)->t$V2
substr(t$V0,3,3)->t$V3
substr(t$V0,4,4)->t$V4
substr(t$V0,5,5)->t$V5
melt(t,id="V0")->t
fread("freqalpha.csv")->t1
merge(t,t1,by.x="value",by.y="V1")->t
t[,.(sum(V2)),.(V0,value)][,.(sum(V1),length(V1)),V0]->wordlist5

print("Start with 'arose'")
round=1
correct="n"
while (correct=="n") {
  print("Enter the answer typing green letters in their places and _ for places with yellow and grey letters. Press enter when you are done.")
  greenbox=scan("stdin",what=character(),n=1)
  greenbox<-gsub("_","[a-z]",greenbox[1],fixed=TRUE)
  print("Type all the grey letters you have identified so far, separating each letter with a comma, and press enter")
  greybox=scan("stdin",what=character(),n=1)
  greybox<-gsub(",","|",greybox,fixed=TRUE)
  print("Type all the yellow letters you have identified so far, separating each letter with a comma. If no yellow letter has been identified, type NA. Press enter at the end.")
  yellowbox=scan("stdin",what=character(),n=1)
  yellowbox<-unlist(strsplit(yellowbox, ",",fixed=TRUE))

  if (round==1) {
    if (is.na(yellowbox)) {
      wordlist5[grep(greenbox,V0),][(grep(greybox,V0,invert=TRUE))][V2==5][order(-V1)] ->suggestions
    } else {
      wordlist5[grep(greenbox,V0),][(grep(greybox,V0,invert=TRUE))][Reduce(`&`, Map(`%like%`, list(V0), yellowbox))][V2==5][order(-V1)]->suggestions
    }
  } else
  {
    if (is.na(yellowbox)) {
      wordlist5[grep(greenbox,V0),][(grep(greybox,V0,invert=TRUE))][order(-V1)]->suggestions
    } else {
      wordlist5[grep(greenbox,V0),][(grep(greybox,V0,invert=TRUE))][Reduce(`&`, Map(`%like%`, list(V0), yellowbox))][order(-V1)]->suggestions
    }
  }
  print(suggestions[c(1:10)])
  print("Type the best suggestion as your next guess.")
  print(" Was it correct (y/n)? ")
  correct=scan("stdin",character(),n=1)
  round<-round+1
}

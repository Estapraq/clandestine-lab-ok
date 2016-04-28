#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# This project is about: Cleaning .txt file and converting it to .csv format.
#Requesting longutuid and latitude from google. And also visualize the data 
#on Oklahoma map.
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Pckages that are needed here 
library("ggmap")#import geocode libraray/visualization

##open the text file 
#assign the name of the text file 
filename <- 'datadrug.txt' 

#check if the name of the file exist open it, otherwise open the browser
if (!file.exists(filename)){
        filename <- file.choose()}

#Read filename line by line
mytxtfile = readLines(filename)

##Clean the data
#delet the three lines in the header
mytxtfile = mytxtfile[-(1:3)]

#this loop is to clean the inside text in mytxtfilenew
len <- length(mytxtfile) # length of the object testfile 
i <- 56 # the first text start at line 56 
while(i < len ){
        j=i+7 # the length of the text is 8 lines: first text begin at i=56, end at j= 56+7
        mytxtfile = mytxtfile[-(i: j)] # delet the 8 lines text
        # The text happens every 62 lines, 
        #and we have "-8" since our file is updated eveytime we delete 8 lines
        i<-(i+62)-8
        #we need to update the "len=file length" everytime we delelet 8 lines text
        len <- length(mytxtfile)
}

#delet the six lines in  the tail
tail.start=length(mytxtfile)-5 # the start of the tail 2402
testfileT= testfile[-(tail.start:length(mytxtfile))] #clean the tail

#clean empty rows in mydata 
mytxtfile[which(mytxtfile=="")]#detect the empty lines in "mydata" object
mytxtfile <- mytxtfile[which(mytxtfile!="")]#delet the empty lines in muy character object "mydata"

##WE START APPLYING REGEX ON testfileTnew  
#regex county
COUNTY <- c()#create a vectore to save the county column 
CITY <- c()#create a vectore to save the city column 
ADDRESS <- c()#create a vectore to save the arress column 
DATE <- c() #create a vectore to save the date column 
for (i in 2:2400){
        #regex county     
        l=i-1
        pattren1<-regexpr("\\w+", mytxtfile[i], perl = T)#to get the second word 
        match1<-regmatches(mytxtfile[i], pattren1)
        COUNTY[l] <-match1
        
        #regex city
        pattren2<-regexpr("\\s\\w+", mytxtfile[i], perl = T)#get the pattren 
        match2 <- regmatches(mytxtfile[i], pattren2)#match the pattren with our line of data
        CITY[l] <- match2
        
        #regex address
        pattren3<-regexpr("\\s\\w+\\s\\w+\\s*(.+)\\s", mytxtfile[i], perl = T)#get the pattren 
        match3<- regmatches(mytxtfile[i], pattren3)#match the pattren with our line of data
        if(length(match3)==0){ # taking care the case if the length is zero
                pattren3<-regexpr("\\s\\w+\\s*(.+)\\s", mytxtfile[i], perl = T)
                match3 <- regmatches(mytxtfile[i], pattren3)   
        }
        ADDRESS[l] <- match3
        
        #regex date
        pattren4<-regexpr("\\d+/\\d+/\\d+", mytxtfile[i], perl = T)#get the pattren 
        match4 <- regmatches(mytxtfile[i], pattren4)#match the pattren with our line of data
        if(length(match4)==0){ # taking care the case if the length is zero
                pattren4 <-regexpr("\\s", mytxtfile[i], perl = T)
                match4 <- regmatches(mytxtfile[i], pattren4)   
        }
        DATE[l] <-match4
}

#bind the four vectors in a data frame data.factor
data.factor <- data.frame(COUNTY, CITY, ADDRESS, DATE)

##write a csv file 
write.csv(data.factor, "Drug-Data.csv", row.names = F )#without rows names

##GEOCODES: we are using ggmap here. It usually streams from google and dsk
#create three vectors to store the geocodes data
lo <- c()#lon
la <- c()#lat
ad <- c()#address

leng <- length(data.factor$ADDRESS) #length of the ADDRESS vector

#for loop to run over all the ADDRESS vector in the data frame data.factor
for(n in 1:leng){
        #"latlona" gives lon, lat, and address info
        gc <- geocode(paste(data.factor$ADDRESS[n], data.factor$CITY[n], ' OKlahoma', sep = ',')
                      , output = "latlona", source= "google")
        print(gc)# print the output
        lo[n] <- gc$lon#assign the longtitude 
        la[n] <- gc$lat#assigned the latitude
        ad[n] <- gc$address#assign the address
}

# combined the vectors lo, la , and ad with the data frame data.factor
data.factor <- cbind(data.factor.cbind, long= lo, lat= la, geocode.address= ad)

#write a csv file 
write.csv(data.factor, "drug-lab-clean.csv", row.names = F )#without rows names

##visualize the points on the Oklahoma map
#giving a randome Lon and Lat in oklahoma to catch the center.
OkLocation = c(Lon=-97.5164, Lat=35.4676)
#requising Oklahoma map. Changing 'maptype' would give different result
OkMap <- get_map(location = OkLocation, source = 'stamen', 
                 maptype = 'toner', crop = FALSE, zoom = 7)

#adding a layers to Oklahoma map
ggmap(OkMap) +
        geom_point(aes(x=data.factor$long, y=data.factor$lat), 
                   data = data.factor, alpha=0.5, color="steelblue", size=1)










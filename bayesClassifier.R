train <- read.csv("/home/yd/classifierPractice/train_users_2.csv",na.strings =c(""),header=T)
#Note: Supposed that there are two kinds of age: real age & born year.(I didn't count born year into the model yet)
train <- train[-which(train$age>130),]
#Factor level preserved
train$language <- as.factor(as.character(train$language)) #dealing with mismatch factor level
train$affiliate_provider <- as.factor(as.character(train$affiliate_provider)) #dealing with mismatch factor level
train$first_affiliate_tracked <- as.factor(as.character(train$first_affiliate_tracked)) #dealing with mismatch factor level
train$first_browser <- as.factor(as.character(train$first_browser)) #dealing with mismatch factor level
#add new col: sorting "age" as dummy variable
train <- transform(train,ageRange=ifelse(age<10,0,ifelse(age>=10&age<20,1,
                                                         ifelse(age>=20&age<30,2,
                                                                ifelse(age>=30&age<40,3,
                                                                       ifelse(age>=40&age<50,4,
                                                                              ifelse(age>=50&age<60,5,
                                                                                     ifelse(age>=60&age<70,6,
                                                                                            ifelse(age>=70&age<80,7,
                                                                                                   ifelse(age>=80&age<90,8,
                                                                                                          ifelse(age>=90&age<100,9,
                                                                                                                 ifelse(age>=100,10,age))))))))))))



# bayesClassifier model ---------------------------------------------------
library(dplyr)
detach("package:plyr", unload=TRUE) 
#bayes prior
pcount <- group_by(train,country_destination)%>%summarise(.,count=n())
prior <- group_by(train,country_destination)%>%summarise(.,prior=n()/nrow(train))
#likelihood: P(feature|Classification)
#P(age_i|C_j)
unique(train$ageRange)
age <- group_by(train,country_destination)%>%summarise(.,age0=length(which(ageRange==0))/length(ageRange),
                                                       age1=length(which(ageRange==1))/length(ageRange),
                                                       age2=length(which(ageRange==2))/length(ageRange),
                                                       age3=length(which(ageRange==3))/length(ageRange),
                                                       age4=length(which(ageRange==4))/length(ageRange),
                                                       age5=length(which(ageRange==5))/length(ageRange),
                                                       age6=length(which(ageRange==6))/length(ageRange),
                                                       age7=length(which(ageRange==7))/length(ageRange),
                                                       age8=length(which(ageRange==8))/length(ageRange),
                                                       age9=length(which(ageRange==9))/length(ageRange),
                                                       age10=length(which(ageRange==10))/length(ageRange)
)
#P(gender_i|C_j)
unique(train$gender)
gender <- group_by(train,country_destination)%>%summarise(.,FEMALE=length(which(gender=="FEMALE"))/length(gender),
                                                          MALE=length(which(gender=="MALE"))/length(gender),
                                                          UNKNOWN=length(which(gender=="-unknown-"))/length(gender),
                                                          OTHER=length(which(gender=="OTHER"))/length(gender))
#P(signup_method_i|C_j)
unique(train$signup_method)
signup_method <- group_by(train,country_destination)%>%summarise(.,basic=length(which(signup_method=="basic"))/length(signup_method),
                                                                 facebook=length(which(signup_method=="facebook"))/length(signup_method),
                                                                 google=length(which(signup_method=="google"))/length(signup_method))
#P(signup_flow_i|C_j)
sort(unique(train$signup_flow))
signup_flow <- group_by(train,country_destination)%>%summarise(.,flow0=length(which(signup_flow==0))/length(signup_flow),
                                                               flow1=length(which(signup_flow==1))/length(signup_flow),
                                                               flow2=length(which(signup_flow==2))/length(signup_flow),
                                                               flow3=length(which(signup_flow==3))/length(signup_flow),
                                                               flow4=length(which(signup_flow==4))/length(signup_flow),
                                                               flow5=length(which(signup_flow==5))/length(signup_flow),
                                                               flow6=length(which(signup_flow==6))/length(signup_flow),
                                                               flow8=length(which(signup_flow==8))/length(signup_flow),
                                                               flow10=length(which(signup_flow==10))/length(signup_flow),
                                                               flow12=length(which(signup_flow==12))/length(signup_flow),
                                                               flow15=length(which(signup_flow==15))/length(signup_flow),
                                                               flow16=length(which(signup_flow==16))/length(signup_flow),
                                                               flow20=length(which(signup_flow==20))/length(signup_flow),
                                                               flow21=length(which(signup_flow==21))/length(signup_flow),
                                                               flow23=length(which(signup_flow==23))/length(signup_flow),
                                                               flow24=length(which(signup_flow==24))/length(signup_flow),
                                                               flow25=length(which(signup_flow==25))/length(signup_flow))
sum(signup_flow[3,2:18])
#P(language_i|C_j)
unique(as.factor(as.character(train$language)))
language <- group_by(train,country_destination)%>%summarise(.,en=length(which(language=="en"))/length(language),
                                                            es=length(which(language=="es"))/length(language),
                                                            fr=length(which(language=="fr"))/length(language),
                                                            zh=length(which(language=="zh"))/length(language),
                                                            ko=length(which(language=="ko"))/length(language),
                                                            de=length(which(language=="de"))/length(language),
                                                            it=length(which(language=="it"))/length(language),
                                                            ja=length(which(language=="ja"))/length(language),
                                                            pt=length(which(language=="pt"))/length(language),
                                                            ru=length(which(language=="ru"))/length(language),
                                                            nl=length(which(language=="nl"))/length(language),
                                                            sv=length(which(language=="sv"))/length(language),
                                                            pl=length(which(language=="pl"))/length(language),
                                                            da=length(which(language=="da"))/length(language),
                                                            fi=length(which(language=="fi"))/length(language),
                                                            tr=length(which(language=="tr"))/length(language),
                                                            no=length(which(language=="no"))/length(language),
                                                            hu=length(which(language=="hu"))/length(language),
                                                            cs=length(which(language=="cs"))/length(language),
                                                            th=length(which(language=="th"))/length(language),
                                                            ca=length(which(language=="ca"))/length(language),
                                                            is=length(which(language=="is"))/length(language),
                                                            id=length(which(language=="id"))/length(language),
                                                            hr=length(which(language=="hr"))/length(language),
                                                            el=length(which(language=="el"))/length(language))
sum(language[3,2:26])

#P(affiliate_channel|C_j)
unique(train$affiliate_channel)
affiliate_channel <- group_by(train,country_destination)%>%summarise(.,direct=length(which(affiliate_channel=="direct"))/length(affiliate_channel),
                                                                     other=length(which(affiliate_channel=="other"))/length(affiliate_channel),
                                                                     seo=length(which(affiliate_channel=="seo"))/length(affiliate_channel),
                                                                     sem_non_brand=length(which(affiliate_channel=="sem-non-brand"))/length(affiliate_channel),
                                                                     content=length(which(affiliate_channel=="content"))/length(affiliate_channel),
                                                                     sem_brand=length(which(affiliate_channel=="sem-brand"))/length(affiliate_channel),
                                                                     remarketing=length(which(affiliate_channel=="remarketing"))/length(affiliate_channel),
                                                                     api=length(which(affiliate_channel=="api"))/length(affiliate_channel)
)
sum(affiliate_channel[2,2:9])

#P(affiliate_provider_i|C_j)
unique(train$affiliate_provider)
affiliate_provider <- group_by(train,country_destination)%>%summarise(.,direct=length(which(affiliate_provider=="direct"))/length(affiliate_provider),
                                                                      craigslist=length(which(affiliate_provider=="craigslist"))/length(affiliate_provider),
                                                                      google=length(which(affiliate_provider=="google"))/length(affiliate_provider),
                                                                      other=length(which(affiliate_provider=="other"))/length(affiliate_provider),
                                                                      facebook=length(which(affiliate_provider=="facebook"))/length(affiliate_provider),
                                                                      vast=length(which(affiliate_provider=="vast"))/length(affiliate_provider),
                                                                      bing=length(which(affiliate_provider=="bing"))/length(affiliate_provider),
                                                                      facebook_open_graph=length(which(affiliate_provider=="facebook-open-graph"))/length(affiliate_provider),
                                                                      email_marketing=length(which(affiliate_provider=="email-marketing"))/length(affiliate_provider),
                                                                      meetup=length(which(affiliate_provider=="meetup"))/length(affiliate_provider),
                                                                      yahoo=length(which(affiliate_provider=="yahoo"))/length(affiliate_provider),
                                                                      padmapper=length(which(affiliate_provider=="padmapper"))/length(affiliate_provider),
                                                                      gsp=length(which(affiliate_provider=="gsp"))/length(affiliate_provider),
                                                                      naver=length(which(affiliate_provider=="naver"))/length(affiliate_provider),
                                                                      baidu=length(which(affiliate_provider=="baidu"))/length(affiliate_provider),
                                                                      wayn=length(which(affiliate_provider=="wayn"))/length(affiliate_provider),
                                                                      daum=length(which(affiliate_provider=="daum"))/length(affiliate_provider),
                                                                      yandex=length(which(affiliate_provider=="yandex"))/length(affiliate_provider))
sum(affiliate_provider[5,2:19])

#P(first_affiliate_tracked_i|C_j)
unique(train$first_affiliate_tracked)
first_affiliate_tracked <- group_by(train,country_destination)%>%summarise(.,untracked=length(which(first_affiliate_tracked=="untracked"))/length(first_affiliate_tracked),
                                                                           omg=length(which(first_affiliate_tracked=="omg"))/length(first_affiliate_tracked),
                                                                           linked=length(which(first_affiliate_tracked=="linked"))/length(first_affiliate_tracked),
                                                                           tracked_other=length(which(first_affiliate_tracked=="tracked-other"))/length(first_affiliate_tracked),
                                                                           product=length(which(first_affiliate_tracked=="product"))/length(first_affiliate_tracked),
                                                                           marketing=length(which(first_affiliate_tracked=="marketing"))/length(first_affiliate_tracked),
                                                                           UNKNOWN=length(which(is.na(first_affiliate_tracked)))/length(first_affiliate_tracked),
                                                                           local_ops=length(which(first_affiliate_tracked=="local ops"))/length(first_affiliate_tracked))
sum(first_affiliate_tracked[5,2:9])

#P(first_affiliate_tracked_i|C_j)
unique(train$signup_app)
signup_app <- group_by(train,country_destination)%>%summarise(.,Web=length(which(signup_app=="Web"))/length(signup_app),
                                                              Moweb=length(which(signup_app=="Moweb"))/length(signup_app),
                                                              iOS=length(which(signup_app=="iOS"))/length(signup_app),
                                                              Android=length(which(signup_app=="Android"))/length(signup_app))
sum(signup_app[1,2:5])

#P(first_device_type_i|C_j)
unique(train$first_device_type)
first_device_type <- group_by(train,country_destination)%>%summarise(.,Windows_Desktop=length(which(first_device_type=="Windows Desktop"))/length(first_device_type),
                                                                     Mac_Desktop=length(which(first_device_type=="Mac Desktop"))/length(first_device_type),
                                                                     iPhone=length(which(first_device_type=="iPhone"))/length(first_device_type),
                                                                     Desktop_Other=length(which(first_device_type=="Desktop (Other)"))/length(first_device_type),
                                                                     Android_Tablet=length(which(first_device_type=="Android Tablet"))/length(first_device_type),
                                                                     iPad=length(which(first_device_type=="iPad"))/length(first_device_type),
                                                                     Android_Phone=length(which(first_device_type=="Android Phone"))/length(first_device_type),
                                                                     Other_Unknown=length(which(first_device_type=="Other/Unknown"))/length(first_device_type),
                                                                     SmartPhone_Other=length(which(first_device_type=="SmartPhone (Other)"))/length(first_device_type))
sum(first_device_type[5,2:10])

#P(first_browser_i|C_j)
unique(train$first_browser)
first_browser <- group_by(train,country_destination)%>%summarise(.,IE=length(which(first_browser=="IE"))/length(first_browser),
                                                                 Firefox=length(which(first_browser=="Firefox"))/length(first_browser),
                                                                 Chrome=length(which(first_browser=="Chrome"))/length(first_browser),
                                                                 Safari=length(which(first_browser=="Safari"))/length(first_browser),
                                                                 UNKNOWN=length(which(first_browser=="-unknown-"))/length(first_browser),
                                                                 Mobile_Safari=length(which(first_browser=="Mobile Safari"))/length(first_browser),
                                                                 Chrome_Mobile=length(which(first_browser=="Chrome Mobile"))/length(first_browser),
                                                                 RockMelt=length(which(first_browser=="RockMelt"))/length(first_browser),
                                                                 Chromium=length(which(first_browser=="Chromium"))/length(first_browser),
                                                                 Android_Browser=length(which(first_browser=="Android Browser"))/length(first_browser),
                                                                 AOL_Explorer=length(which(first_browser=="AOL Explorer"))/length(first_browser),
                                                                 Palm_Pre_web_browser=length(which(first_browser=="Palm Pre web browser"))/length(first_browser),
                                                                 Mobile_Firefox=length(which(first_browser=="Mobile Firefox"))/length(first_browser),
                                                                 TenFourFox=length(which(first_browser=="TenFourFox"))/length(first_browser),
                                                                 Opera=length(which(first_browser=="Opera"))/length(first_browser),
                                                                 IE_Mobile=length(which(first_browser=="IE Mobile"))/length(first_browser),
                                                                 Apple_Mail=length(which(first_browser=="Apple Mail"))/length(first_browser),
                                                                 Arora=length(which(first_browser=="Arora"))/length(first_browser),
                                                                 Silk=length(which(first_browser=="Silk"))/length(first_browser),
                                                                 Camino=length(which(first_browser=="Camino"))/length(first_browser),
                                                                 BlackBerry_Browser=length(which(first_browser=="BlackBerry Browser"))/length(first_browser),
                                                                 SeaMonkey=length(which(first_browser=="SeaMonkey"))/length(first_browser),
                                                                 IE_Mobile=length(which(first_browser=="IE Mobile"))/length(first_browser),
                                                                 Kindle_Browser=length(which(first_browser=="Kindle Browser"))/length(first_browser),
                                                                 CoolNovo=length(which(first_browser=="CoolNovo"))/length(first_browser),
                                                                 Conkeror=length(which(first_browser=="Conkeror"))/length(first_browser),
                                                                 wOSBrowser=length(which(first_browser=="wOSBrowser"))/length(first_browser),
                                                                 Google_Earth=length(which(first_browser=="Google Earth"))/length(first_browser),
                                                                 Crazy_Browser=length(which(first_browser=="Crazy Browser"))/length(first_browser),
                                                                 Maxthon=length(which(first_browser=="Maxthon"))/length(first_browser),
                                                                 Mozilla=length(which(first_browser=="Mozilla"))/length(first_browser),
                                                                 OmniWeb=length(which(first_browser=="OmniWeb"))/length(first_browser),
                                                                 PS_Vita_browser=length(which(first_browser=="PS Vita browser"))/length(first_browser),
                                                                 NetNewsWire=length(which(first_browser=="NetNewsWire"))/length(first_browser),
                                                                 CometBird=length(which(first_browser=="CometBird"))/length(first_browser),
                                                                 Comodo_Dragon=length(which(first_browser=="Comodo Dragon"))/length(first_browser),
                                                                 Flock=length(which(first_browser=="Flock"))/length(first_browser),
                                                                 Pale_Moon=length(which(first_browser=="Pale Moon"))/length(first_browser),
                                                                 Avant_Browser=length(which(first_browser=="Avant Browser"))/length(first_browser),
                                                                 Opera_Mobile=length(which(first_browser=="Opera Mobile"))/length(first_browser),
                                                                 TheWorld_Browser=length(which(first_browser=="TheWorld Browser"))/length(first_browser),
                                                                 Iron=length(which(first_browser=="Iron"))/length(first_browser),
                                                                 Sogou_Explorer=length(which(first_browser=="Sogou Explorer"))/length(first_browser),
                                                                 IceWeasel=length(which(first_browser=="IceWeasel"))/length(first_browser),
                                                                 SlimBrowser=length(which(first_browser=="SlimBrowser"))/length(first_browser),
                                                                 Epic=length(which(first_browser=="Epic"))/length(first_browser),
                                                                 Googlebot=length(which(first_browser=="Googlebot"))/length(first_browser),
                                                                 Outlook_2007=length(which(first_browser=="Outlook 2007"))/length(first_browser),
                                                                 IceDragon=length(which(first_browser=="IceDragon"))/length(first_browser),
                                                                 SiteKiosk=length(which(first_browser=="SiteKiosk"))/length(first_browser),
                                                                 Maxthon=length(which(first_browser=="Maxthon"))/length(first_browser),
                                                                 Stainless=length(which(first_browser=="Stainless"))/length(first_browser),
                                                                 Opera_Mini=length(which(first_browser=="Opera Mini"))/length(first_browser),
                                                                 Yandex.Browser=length(which(first_browser=="Yandex.Browser"))/length(first_browser))
sum(first_browser[1,2:53])

#combine prior & likelihood
combine <- list()
combine$prior <- prior
combine$gender <- gender
combine$age <- age #the test data must transform to the ageRange
combine$language <- language
combine$affiliate_channel <- affiliate_channel
combine$affiliate_provider <- affiliate_provider
combine$first_browser <- first_browser
combine$first_device_type <- first_device_type
combine$first_affiliate_tracked <- first_affiliate_tracked
combine$signup_app <- signup_app
combine$signup_flow <- signup_flow
combine$signup_method <- signup_method

# Testing-set Transformation ----------------------------------------------
#Fitting the names of classification into the Bayesmodel
#testing data import
test <- read.csv("/home/yd/classifierPractice/test_users.csv",na.strings =c(""),header=T)
library(plyr)
#gender
colnames(combine$gender)[-1]
unique(test$gender)
test$gender <- revalue(test$gender,c("-unknown-"="UNKNOWN"))
match(colnames(combine$gender)[-1],unique(test$gender))
#age newfactor:NA
colnames(combine$age)[-1]
unique(test$age)
test$age <- sapply(1:nrow(test),function(x){
  ifelse(test$age[x]<10,"age0",ifelse(test$age[x]>=10&test$age[x]<20,"age1",
                                      ifelse(test$age[x]>=20&test$age[x]<30,"age2",
                                             ifelse(test$age[x]>=30&test$age[x]<40,"age3",
                                                    ifelse(test$age[x]>=40&test$age[x]<50,"age4",
                                                           ifelse(test$age[x]>=50&test$age[x]<60,"age5",
                                                                  ifelse(test$age[x]>=60&test$age[x]<70,"age6",
                                                                         ifelse(test$age[x]>=70&test$age[x]<80,"age7",
                                                                                ifelse(test$age[x]>=80&test$age[x]<90,"age8",
                                                                                       ifelse(test$age[x]>=90&test$age[x]<100,"age9",
                                                                                              ifelse(test$age[x]>=100,"age10",test$age[x])))))))))))
})
match(colnames(combine$age)[-1],unique(test$age))
#language new factor: -unknown- 
colnames(combine$language)[-1]
unique(test$language)
match(colnames(combine$language)[-1],unique(test$language))
#affiliate_channel: no "api" in testing data
colnames(combine$affiliate_channel)[-1]
unique(test$affiliate_channel)
test$affiliate_channel <- revalue(test$affiliate_channel,c("sem-non-brand"="sem_non_brand","sem-brand"="sem_brand"))
match(colnames(combine$affiliate_channel)[-1],unique(test$affiliate_channel))

#affiliate_provider: testing no wayn
colnames(combine$affiliate_provider)[-1]
unique(test$affiliate_provider)
test$affiliate_provider <- revalue(test$affiliate_provider,c("email-marketing"="email_marketing","facebook-open-graph"="facebook_open_graph"))
match(colnames(combine$affiliate_provider)[-1],unique(test$affiliate_provider))

#first_browser: training no Ibrowse, Nintendo Browser, UC Browser
colnames(combine$first_browser)[-1]
unique(test$first_browser)
test$first_browser <- revalue(test$first_browser,c("-unknown-"="UNKNOWN",
                                                   "Mobile Safari"="Mobile_Safari",
                                                   "Android Browser"="Android_Browser",
                                                   "Chrome Mobile"="Chrome_Mobile",
                                                   "AOL Explorer"="AOL_Explorer",
                                                   "Mobile Firefox"="Mobile_Firefox",
                                                   "Apple Mail"="Apple_Mail",
                                                   "Google Earth"="Google_Earth",
                                                   "Crazy Browser"="Crazy_Browser",
                                                   "BlackBerry Browser"="BlackBerry_Browser",
                                                   "IE Mobile"="IE_Mobile",
                                                   "Comodo Dragon"="Comodo_Dragon",
                                                   "Opera Mobile"="Opera_Mobile",
                                                   "PS Vita browser"="PS_Vita_browser",
                                                   "Kindle Browser"="Kindle_Browser",
                                                   "Palm Pre web browser"="Palm_Pre_web_browser",
                                                   "Sogou Explorer"="Sogou_Explorer",
                                                   "Avant Browser"="Avant_Browser",
                                                   "Outlook 2007"="Outlook_2007",
                                                   "Pale Moon"="Pale_Moon",
                                                   "TheWorld Browser"="TheWorld_Browser",
                                                   "Opera Mini"="Opera_Mini"))
match(unique(test$first_browser),colnames(combine$first_browser)[-1])
colnames(combine$first_browser)[-1][is.na(match(colnames(combine$first_browser)[-1],unique(test$first_browser)))]

#first_device_type
colnames(combine$first_device_type)[-1]
unique(test$first_device_type)
test$first_device_type <- revalue(test$first_device_type,c("Windows Desktop"="Windows_Desktop","Mac Desktop"="Mac_Desktop","Android Tablet"="Android_Tablet","Android Phone"="Android_Phone","Desktop (Other)"="Desktop_Other","Other/Unknown"="Other_Unknown","SmartPhone (Other)"="SmartPhone_Other"))
match(colnames(combine$first_device_type)[-1],unique(test$first_device_type))

#first_affiliate_tracked: is no match then drop to UNKNOWN
colnames(combine$first_affiliate_tracked)[-1]
unique(test$first_affiliate_tracked)
test$first_affiliate_tracked <- revalue(test$first_affiliate_tracked,c("tracked-other"="tracked_other","local ops"="local_ops","UNKNOWN"='NA'))
match(colnames(combine$first_affiliate_tracked)[-1],unique(test$first_affiliate_tracked))


#signup_app
colnames(combine$signup_app)[-1]
unique(test$signup_app)
match(colnames(combine$signup_app)[-1],unique(test$signup_app))


#signup_method
colnames(combine$signup_method)[-1]
unique(test$signup_method)
match(colnames(combine$signup_method)[-1],unique(test$signup_method))


#signup_flow: there is no "14" in the testing set
colnames(combine$signup_flow)[-1]
unique(test$signup_flow)
test$signup_flow <- sapply(1:nrow(test),function(x){
  ifelse(test$signup_flow[x]==0,"flow0",
         ifelse(test$signup_flow[x]==1,"flow1",
                ifelse(test$signup_flow[x]==2,"flow2",
                       ifelse(test$signup_flow[x]==3,"flow3",
                              ifelse(test$signup_flow[x]==4,"flow4",
                                     ifelse(test$signup_flow[x]==5,"flow5",
                                            ifelse(test$signup_flow[x]==6,"flow6",
                                                   ifelse(test$signup_flow[x]==8,"flow8",
                                                          ifelse(test$signup_flow[x]==10,"flow10",
                                                                 ifelse(test$signup_flow[x]==12,"flow12",
                                                                        ifelse(test$signup_flow[x]==15,"flow15",
                                                                               ifelse(test$signup_flow[x]==16,"flow16",
                                                                                      ifelse(test$signup_flow[x]==20,"flow20",
                                                                                             ifelse(test$signup_flow[x]==21,"flow21",
                                                                                                    ifelse(test$signup_flow[x]==23,"flow23",
                                                                                                           ifelse(test$signup_flow[x]==24,"flow24",
                                                                                                                  ifelse(test$signup_flow[x]==25,"flow25",test$signup_flow[x])))))))))))))))))
})


match(colnames(combine$signup_flow)[-1],unique(test$signup_flow))

# Function of bayesClassifier ---------------------------------------------
bayesClassifier <- function(data=test,method="default"){
  result.id <- vector()
  result.country <- vector()
  for(idIdx in 1:nrow(data)){
    data <- test[idIdx,c(1,5:15)]
    id <- data$id
    #label
    label <- combine$prior[1]
    #prior
    if(method=="softmax") {
      exp.prior <- exp(combine$prior[2])/sum(exp(combine$prior[2]))
      #feature:gender 
      if(is.na(match(data$gender,colnames(combine$gender)))==F){
        exp.gender <- exp(combine$gender[match(data$gender,colnames(combine$gender))])/sum(exp(combine$gender[match(data$gender,colnames(combine$gender))]))
      }else{
        exp.gender <- 0
      }
      #feature:age  NEW FACTOR(to be log(1)): NA  
      if(is.na(match(data$age,colnames(combine$age)))==F){
        exp.age <- exp(combine$age[match(data$age,colnames(combine$age))])/sum(exp(combine$age[match(data$age,colnames(combine$age))]))
      }else{
        exp.age <- 0
      }
      
      #feature:language NEW FACTOR(to be log(1)): -unknown-
      if(is.na(match(data$language,colnames(combine$language)))==F){
        exp.language <- exp(combine$language[match(data$language,colnames(combine$language))])/sum(exp(combine$language[match(data$language,colnames(combine$language))]))
      }else{
        exp.language <- 0
      }
      #feature:affiliate_channel: no "api" in testing data
      if(is.na(match(data$affiliate_channel,colnames(combine$affiliate_channel)))==F){
        exp.affliate_channel <- exp(combine$affiliate_channel[match(data$affiliate_channel,colnames(combine$affiliate_channel))])/sum(exp(combine$affiliate_channel[match(data$affiliate_channel,colnames(combine$affiliate_channel))]))
      }else{
        exp.affliate_channel <- 0
      }
      #feature:affiliate_provider: testing no wayn
      if(is.na(match(data$affiliate_provider,colnames(combine$affiliate_provider)))==F){
        exp.affiliate_provider <- exp(combine$affiliate_provider[match(data$affiliate_provider,colnames(combine$affiliate_provider))])/sum(exp(combine$affiliate_provider[match(data$affiliate_provider,colnames(combine$affiliate_provider))]))
      }else{
        exp.affiliate_provider <- 0
      }
      #first_browser: training no Ibrowse, Nintendo Browser, UC Browser
      if(is.na(match(data$first_browser,colnames(combine$first_browser)))==F){
        exp.first_browser <- exp(combine$first_browser[match(data$first_browser,colnames(combine$first_browser))])/sum(exp(combine$first_browser[match(data$first_browser,colnames(combine$first_browser))]))
      }else{
        exp.first_browser <- 0
      }
      #first_device_type:
      if(is.na(match(data$first_device_type,colnames(combine$first_device_type)))==F){
        exp.first_device_type <- exp(combine$first_device_type[match(data$first_device_type,colnames(combine$first_device_type))])/sum(exp(combine$first_device_type[match(data$first_device_type,colnames(combine$first_device_type))]))
      }else{
        exp.first_device_type <- 0
      }
      #first_affiliate_tracked: is no match then drop to UNKNOWN
      if(is.na(match(data$first_affiliate_tracked,colnames(combine$first_affiliate_tracked)))==F){
        exp.first_affiliate_tracked <- exp(combine$first_affiliate_tracked[match(data$first_affiliate_tracked,colnames(combine$first_affiliate_tracked))])/sum(exp(combine$first_affiliate_tracked[match(data$first_affiliate_tracked,colnames(combine$first_affiliate_tracked))]))
      }else{
        exp.first_affiliate_tracked <- exp(combine$first_affiliate_tracked["UNKNOWN"])/sum(exp(combine$first_affiliate_tracked["UNKNOWN"]))
      }
      #signup_app
      if(is.na(match(data$signup_app,colnames(combine$signup_app)))==F){
        exp.signup_app <- exp(combine$signup_app[match(data$signup_app,colnames(combine$signup_app))])/sum(exp(combine$signup_app[match(data$signup_app,colnames(combine$signup_app))]))
      }else{
        exp.signup_app <- 0
      }
      #signup_method
      if(is.na(match(data$signup_method,colnames(combine$signup_method)))==F){
        exp.signup_method <- exp(combine$signup_method[match(data$signup_method,colnames(combine$signup_method))])/sum(exp(combine$signup_method[match(data$signup_method,colnames(combine$signup_method))]))
      }else{
        exp.signup_method <- 0
      }
      #signup_flow: there is no "14" in the testing set
      if(is.na(match(data$signup_flow,colnames(combine$signup_flow)))==F){
        exp.signup_flow <- exp(combine$signup_flow[match(data$signup_flow,colnames(combine$signup_flow))])/sum(exp(combine$signup_flow[match(data$signup_flow,colnames(combine$signup_flow))]))
      }else{
        exp.signup_flow <- 0
      }
      sum <- 
        exp.prior+
        exp.gender+
        exp.age+
        exp.language+
        exp.affliate_channel+
        exp.affiliate_provider+
        exp.first_affiliate_tracked+
        exp.first_device_type+
        exp.first_browser+
        exp.signup_app+
        exp.signup_flow+
        exp.signup_method
    }
    if(method=="default") {
      log.prior <- log(combine$prior[2])
      #feature:gender 
      if(is.na(match(data$gender,colnames(combine$gender)))==F){
        log.gender <- log(combine$gender[match(data$gender,colnames(combine$gender))])
      }else{
        log.gender <- log(1)
      }
      #feature:age  NEW FACTOR(to be log(1)): NA  
      if(is.na(match(data$age,colnames(combine$age)))==F){
        log.age <- log(combine$age[match(data$age,colnames(combine$age))])
      }else{
        log.age <- log(1)
      }
      #feature:language NEW FACTOR(to be log(1)): -unknown-
      if(is.na(match(data$language,colnames(combine$language)))==F){
        log.language <- log(combine$language[match(data$language,colnames(combine$language))])
      }else{
        log.language <- log(1)
      }
      #feature:affiliate_channel: no "api" in testing data
      if(is.na(match(data$affiliate_channel,colnames(combine$affiliate_channel)))==F){
        log.affliate_channel <- log(combine$affiliate_channel[match(data$affiliate_channel,colnames(combine$affiliate_channel))])
      }else{
        log.affliate_channel <- log(1)
      }
      #feature:affiliate_provider: testing no wayn
      if(is.na(match(data$affiliate_provider,colnames(combine$affiliate_provider)))==F){
        log.affiliate_provider <- log(combine$affiliate_provider[match(data$affiliate_provider,colnames(combine$affiliate_provider))])
      }else{
        log.affiliate_provider <- log(1)
      }
      #first_browser: training no Ibrowse, Nintendo Browser, UC Browser
      if(is.na(match(data$first_browser,colnames(combine$first_browser)))==F){
        log.first_browser <- log(combine$first_browser[match(data$first_browser,colnames(combine$first_browser))])
      }else{
        log.first_browser <- log(1)
      }
      #first_device_type:
      if(is.na(match(data$first_device_type,colnames(combine$first_device_type)))==F){
        log.first_device_type <- log(combine$first_device_type[match(data$first_device_type,colnames(combine$first_device_type))])
      }else{
        log.first_device_type <- log(1)
      }
      #first_affiliate_tracked: is no match then drop to UNKNOWN
      if(is.na(match(data$first_affiliate_tracked,colnames(combine$first_affiliate_tracked)))==F){
        log.first_affiliate_tracked <- log(combine$first_affiliate_tracked[match(data$first_affiliate_tracked,colnames(combine$first_affiliate_tracked))])
      }else{
        log.first_affiliate_tracked <- log(combine$first_affiliate_tracked["UNKNOWN"])
      }
      #signup_app
      if(is.na(match(data$signup_app,colnames(combine$signup_app)))==F){
        log.signup_app <- log(combine$signup_app[match(data$signup_app,colnames(combine$signup_app))])
      }else{
        log.signup_app <- log(1)
      }
      #signup_method
      if(is.na(match(data$signup_method,colnames(combine$signup_method)))==F){
        log.signup_method <- log(combine$signup_method[match(data$signup_method,colnames(combine$signup_method))])
      }else{
        log.signup_method <- log(1)
      }
      #signup_flow: there is no "14" in the testing set
      if(is.na(match(data$signup_flow,colnames(combine$signup_flow)))==F){
        log.signup_flow <- log(combine$signup_flow[match(data$signup_flow,colnames(combine$signup_flow))])
      }else{
        log.signup_flow <- log(1)
      }
      sum <- 
        log.prior+
        log.gender+
        log.age+
        log.language+
        log.affliate_channel+
        log.affiliate_provider+
        log.first_affiliate_tracked+
        log.first_device_type+
        log.first_browser+
        log.signup_app+
        log.signup_flow+
        log.signup_method 
    }
    df<- cbind(label,sum)
    top5 <- df$country_destination[order(df[[2]],decreasing = T)][1:5]
    (result.id <- c(result.id,as.character(rep(id,5))))
    (result.country <- c(result.country,as.character(top5)))
  }
  return(cbind(result.id,result.country))
}

# Testing two method: defualt vs. softmax rule ----------------------------
#scores:0.85065
 result <- bayesClassifier(data=test,method="default")
 colnames(result) <- c("id","country")
 write.csv(result,"/home/yd/classifierPractice/log_result.csv",row.names = F)

#score:0.839
 system.time(result_Soft <- bayesClassifier(data=test,method="softmax"))
 colnames(result_Soft) <- c("id","country")
 write.csv(result_Soft,"/home/yd/classifierPractice/Soft_result.csv",row.names = F)



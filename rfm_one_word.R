require(ggplot2)
require(rfm)
require(lubridate)
require(dplyr)
require(tidyr)
require(stringr)
require(openxlsx)
require(arsenal)
require(compare)
require(sqldf)
require(diffobj)
require(data.table)
require(fastmatch)

if (file.exists("One Word Triennale.xlsx"))

  Bellissima_IT_Triennale <- read_excel("One Word Triennale.xlsx", 
                                      sheet = "Bellissima IT Triennale - Resi")

head(Bellissima_IT_Triennale)

names(Bellissima_IT_Triennale)

 ##Primo test aggregazione, numero ordine, punto consegna desc, data 100670,4

aggregato_per_ordine1 <- aggregate(Bellissima_IT_Triennale$Totale_di_riga_imponibile, 
                                   by=list(Bellissima_IT_Triennale$Ord_Num, 
                                           Bellissima_IT_Triennale$Punto_Consegna_Descr, 
                                           Bellissima_IT_Triennale$Ord_Data), FUN=sum)

##uguale al primo ma con la colonna extra per numero di pezzi*prezzo unitario

aggregato_per_ordine1.1 <- aggregate(Bellissima_IT_Triennale$prezzo.totale, 
                                     by=list(Bellissima_IT_Triennale$Ord_Num, 
                                             Bellissima_IT_Triennale$Punto_Consegna_Descr, 
                                             Bellissima_IT_Triennale$Ord_Data), FUN=sum)

 ##Secondo test aggregazione, numero di ordine, punto di consegna mail, data 1000593, 4


aggregato_per_ordine2 <- aggregate(Bellissima_IT_Triennale$Totale_di_riga_imponibile, 
                                   by= list(Bellissima_IT_Triennale$Ord_Num, 
                                            Bellissima_IT_Triennale$Punto_Consegna_Mail, 
                                            Bellissima_IT_Triennale$Ord_Data), FUN =sum)

##Terzo test per aggregare, numero di ordine, punto di consegna mail, punto di consegna desc, data 100591,5

aggregato_per_ordine3 <- aggregate(Bellissima_IT_Triennale$Totale_di_riga_imponibile, 
                                   by= list(Bellissima_IT_Triennale$Ord_Num, 
                                            Bellissima_IT_Triennale$Punto_Consegna_Mail, 
                                            Bellissima_IT_Triennale$Ord_Data, 
                                            Bellissima_IT_Triennale$Punto_Consegna_Descr), FUN =sum)

##Verifica delle differenze tra le forme di aggregazione utilizzate

a1NotIna2_6<- aggregato_per_ordine1$Group.1 %fin% aggregato_per_ordine2$Group.1
a1NotIna2_6_1 <- aggregato_per_ordine1[which(a1NotIna2_6 == FALSE)]

##sono risultate 79 Righe di Differenza, solo 2 clienti e il resto sono acquisti generici sotto nome Ecommerce BE IT


#Da qui in poi si procederà solamente con il aggregato_per_ordine1 come base per il pacchetto
#RFM

#fissaggio data ancora per calcolare i livelli di recency 

analysis_date <- lubridate::as_date("2020-06-23", tz = "UTC")

aggregato_per_ordine1_lubridate <- aggregato_per_ordine1

aggregato_per_ordine1_lubridate$lubridata <- ymd(aggregato_per_ordine1_lubridate$Group.3)


names(aggregato_per_ordine1_lubridate)[1]<- "order.number"
names(aggregato_per_ordine1_lubridate)[2]<- "customer_id"
names(aggregato_per_ordine1_lubridate)[3]<- "order_date"
names(aggregato_per_ordine1_lubridate)[3]<- "order_date_errato"
names(aggregato_per_ordine1_lubridate)[5]<- "order_date"
names(aggregato_per_ordine1_lubridate)[4]<- "revenue"

#the rfm_table_order doesn't like NA so we are going to check how many we have


NAs <- which(!complete.cases(aggregato_per_ordine1_lubridate))

#metodo analogo usando la funzione di dplyr 

NAs_2 <- aggregato_per_ordine1_lubridate %>%
          select(everything()) %>%   summarise_all(funs(sum(is.na(.))))

#ci sono 945 valori economici "revenue" assenti, procediamo a dropparli 
#con la funzione drop_na()


#anlisi con quartili per gli scoring

rfm_result <- rfm_table_order(drop_na(aggregato_per_ordine1_lubridate), customer_id = customer_id, order_date = order_date, revenue = revenue, analysis_date = analysis_date, recency_bins = 4, frequency_bins = 4, monetary_bins = 4)

rfm_result[1:10,]

##primo output-grezzo 

write.table(rfm_result$rfm, file = "pre_segmentazione_rfm.csv")

##Segmentazione metodo: https://rpubs.com/omerperach/RFM_IDC


champions<- c(444)
loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
potential_loyalist <-c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
recent_customers <- c(411)
promising <- c(311, 312, 313, 331)
needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
about_to_sleep <- c(211)
at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
cant_lose <- c(134,143,144,234,242,243,244)
hibernating <- c(141)
lost <- c(111)

rfm_score <- rfm_result

rfm_scores<-as.vector(rfm_score$rfm$rfm_score)

rfm_scores[which(rfm_score$rfm$rfm_score %in% champions)]="Champions"
rfm_scores[which(rfm_score$rfm$rfm_score %in% champions)]="Champions"
rfm_scores[which(rfm_scores %in% potential_loyalist)] = "Potential Loyalist"
rfm_scores[which(rfm_scores %in% loyal_customers)] = "Loyal Customers"
rfm_scores[which(rfm_scores %in% recent_customers)] = "Recent Customers"
rfm_scores[which(rfm_scores %in% promising)] = "Promising"
rfm_scores[which(rfm_scores %in% needing_attention)] = "Customer Needing Attention"
rfm_scores[which(rfm_scores %in% about_to_sleep)] = "About to Sleep"
rfm_scores[which(rfm_scores %in% at_risk)] = "At Risk"
rfm_scores[which(rfm_scores %in% cant_lose)] = "Can't Lose Them"
rfm_scores[which(rfm_scores %in% hibernating)] = "Hibernating"
rfm_scores[which(rfm_scores %in% lost)] = "Lost"

customer_segment<-data.frame(cus_seg=rfm_scores)

customer_segment%>%count(cus_seg)%>%arrange(desc(n))%>%rename(cus_seg = cus_seg, Count = n)


ggplot(data = customer_segment) + aes(x = cus_seg, fill = cus_seg)+ geom_bar() + labs(title = "Customer Segmentation", x = "Segment", y = "Total Customer") + coord_flip()+ theme_minimal()

rfm_score2 <- rfm_score

#binding segment to the initial RFM data frame

rfm_score2 <- rfm_score2$rfm %>%mutate(Segment_name=customer_segment$cus_seg)

cross_checking_segments_number <- as.data.frame(table(rfm_score2$Segment_name))
sum(cross_checking_segments_number$Freq)

##90260 corrispondenza per verifica

write.xlsx(rfm_score2, file = "segmentazione_triennale_bellissima_matrice_RFM_no_verifica.xlsx")

write.xlsx(cross_checking_segments_number, file = "segment_amount.xlsx")

##test con funzione integrata segmentazione 

segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)


rfm_segment_test_function_auto <- rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
            frequency_lower, frequency_upper, monetary_lower, monetary_upper)

cross_checking_segment_auto <- as.data.frame(table(rfm_segment_test_function_auto$segment))

sum(cross_checking_segment_auto$Freq)

##90260 Il totale è lo stesso ma rimangono fuori dalla categorizzazione 20k utenti, l'utilizzo del metodo manuale
##permette invece di inserire anche quei segmenti che rimangono fuori

rfm_score2_minus_consegna_BE <- subset(rfm_score2, transaction_count != 77)

transaction_count <- as.data.frame(table(as.data.frame(rfm_score2_minus_consegna_BE)))


##verifica matrice con funzione integrata nel pacchetto DONE


### Inizio verifica sui prodotti venduti/combinazioni e correlazioni

conteggio_sku_prodotti_aggregato <- aggregate(Bellissima_IT_Triennale$Quantità,
                                              by=list(Bellissima_IT_Triennale$Articolo), FUN = sum)


conteggio_sku_prodotti_aggregato_minus_incasso <- conteggio_sku_prodotti_aggregato[1:251,]


names(conteggio_sku_prodotti_aggregato_minus_incasso)[1]<- "Articolo"

merge_per_sku_descrizione_prodotti <- merge(x=conteggio_sku_prodotti_aggregato_minus_incasso, y=aggiunta_descrizione_prodotti, by="Articolo")

sku_descrizione <- distinct(merge_per_sku_descrizione_prodotti, merge_per_sku_descrizione_prodotti$Articolo,
                            merge_per_sku_descrizione_prodotti$Articolo_descr., merge_per_sku_descrizione_prodotti$x)

write.xlsx(sku_descrizione, file = "sku_aggregati_descrizione.xlsx")


only_diffon_customers <- Bellissima_IT_Triennale[Bellissima_IT_Triennale$Articolo_descr. %like% "DIFFUSORE", ]

write.xlsx(only_diffon_customers, file = "only_diffon_customers.xlsx")

diffon_customers_id <- subset(only_diffon_customers, select = c(36, 39, 43,44, 47))

diffon_customers_id_1 <- diffon_customers_id[,1]

diffon_correlati <- Bellissima_IT_Triennale[!Bellissima_IT_Triennale$Punto_Consegna_Descr %in% diffon_customers_id_1,]


correlati_2 <- diffon_correlati_1[!grepl("DIFFUSORE", diffon_correlati_1$Articolo_descr.),]

correlati_2_unique <- distinct(correlati_2_subset)

correlati_alt <- as.data.frame(table(correlati_2_subset$Articolo))

correlati_alt_merge <- merge(correlati_alt, correlati_2_subset, by="Articolo")

correlati_alt_merge_distinct <- distinct(correlati_alt_merge)

write.xlsx(correlati_alt_merge_distinct, file = "prodotti_acquistati_insieme_al_diffon.xlsx")



##Lifetime Values

##Diffon Buyers hanno comprato
##Twist and Style Buyers hanno comprato
#segmentazione clienti pre 2020 
#segmentazione clienti post 2020
#ipotizzo matrice RFM per questi

## Distribuzione Geografica

##



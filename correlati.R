require(data.table)
orders.per.customer <- dt[,sum(Prezzo_unitario), by="Articolo,Punto_Consegna_Descr"]
my.summaries <- merge(orders.per.customer[, length(Articolo), by = Punto_Consegna_Descr],orders.per.customer[, mean(V1), by=Punto_Consegna_Descr], by = "Punto_consegna_Descr")



###funzionante 

my.summaries <- data.frame(customerID= unique(bell$Punto_Consegna_Mail), NumberofOrdersofSpecificUser = sapply(unique(bell$Punto_Consegna_Mail), function(customer) {length(unique(bell$Ord_Data[which(bell$Punto_Consegna_Mail == customer)]))} ),AvergaValuePerOrder = tapply(tapply(bell$Prezzo_unitario, bell$Ord_Num, sum), bell$Punto_Consegna_Mail[match(unique(bell$Ord_Num), bell$Ord_Num)], mean))

##correlazione tra i prodotti 

test<- bell %>% group_by(Punto_Consegna_Mail) %>% summarise(new_col = if(n()==1) list(as.character(Articolo)) else list(combn(sort(Articolo),2,paste0, collapse = ","))) %>% unnest() %>% separate(new_col, c("ArticoloX", "ArticoloY"), sep= ",", fill = "right") %>% na.omit %>% count(ArticoloX, ArticoloY)


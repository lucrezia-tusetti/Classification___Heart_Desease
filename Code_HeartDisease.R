#####
library(corrplot) 
library(ggplot2)
library(gridExtra)
library(heplots) 
library(MASS)
library(tidyverse) 
library(car)
library(class)
library(caret)
library(ROCR)
library(readxl)
library(LabRS)
#####

data <- read.csv("heart.csv",header=T) 


########### FASE 1. ANALISI PRELIMINARE ############

### VERIFICA DELLA TIPOLOGIA DI VARIABILI

# Trasformiamo le variabili qualitative in factor

str(data)
data$Sex <- as.factor(data$Sex)
data$ChestPainType <- as.factor(data$ChestPainType)
data$FastingBS <- as.factor(data$FastingBS)
data$RestingECG <- as.factor(data$RestingECG)
data$ExerciseAngina <- as.factor(data$ExerciseAngina)
data$ST_Slope <- as.factor(data$ST_Slope)
data$HeartDisease <- as.factor(data$HeartDisease)
str(data)


### DIVIDIAMO IN TRAINING, VALIDATION E TEST

train_size <- floor(0.80*nrow(data))
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = train_size)
training <- data[train_index, ]
test <- data[-train_index, ]

subtrain_size <- floor(0.65*nrow(training))
subtrain_index <- sample(seq_len(nrow(training)), size = subtrain_size)
sub_training <- training[subtrain_index, ]
sub_training_original <- sub_training
validation <- training[-subtrain_index, ]
validation_original <- validation


### IL PROBLEMA E' BILANCIATO?

# Vediamo se le proporzioni tra le due classi coincidono nei vari dataset
round(prop.table(table(sub_training$HeartDisease)), 2)
round(prop.table(table(validation$HeartDisease)), 2)
round(prop.table(table(test$HeartDisease)), 2)
# bilanciato


# STATISTICA DESCRITTIVA
# Diagrammi a barre, da valutare dopo aver svolto le analisi esplorative

which_row <- which(data$Cholesterol==0)

data_nozero <- data[-which_row,]

for (i in which_row) {
  data[i,]$Cholesterol <- median(data_nozero$Cholesterol)
}

which(data$Cholesterol==0)

which(data$RestingBP == 0)

dati_new <- data[-450,]


## grafici variabili

dati_new %>% 
  ggplot(aes(x = Sex)) +
  geom_bar(fill = c("#52BE80", "#A9DFBF")) +
  ylab("Frequenza assoluta")


dati_new %>% 
  ggplot(aes(x = ChestPainType)) +
  geom_bar(fill = c("#5B2C6F", "#8E44AD", "#A569BD", "#D2B4DE")) +
  ylab("Frequenza assoluta")


dati_new %>% 
  ggplot(aes(x = HeartDisease)) +
  geom_bar(fill = c("#641E16", "#CD6155")) +
  ylab("Frequenza assoluta")




############# FASE 2. ANALISI ESPLORATIVE ################

### VERIFICA MISSING 

# Controlliamo le principali statistiche per le variabili quantitative. 
# Questo permette di verificare anche la presenza di missing value.
summary(sub_training[, c(1:11)])

# Dal summary vediamo che non ci sono missing 

# Notiamo pero' che sia Colesterol che RestingBP assumono valori pari a 0
# Dal momento che cio' non e' possibile, desumiamo che questi valori non siano
# stati rilevati per questi individui

length(which(sub_training$Cholesterol==0)) # 84 -> decidiamo di imputare il valore mediano
# (data l'elevata presenza di outliers e il fatto che la variabile non sia 
# correlata significativamente con nessun'altra covariata)
length(which(sub_training$RestingBP==0)) #1 -> possiamo eliminare questa osservazione

#datinozero <- data[-which(data$Cholesterol==0),]
#boxplot(datinozero$Cholesterol)

#median(datinozero[datinozero$HeartDisease==1,]$Cholesterol)
#median(datinozero[datinozero$HeartDisease==0,]$Cholesterol)

#median(datinozero[datinozero$Sex=="M",]$Cholesterol)
#median(datinozero[datinozero$Sex=="F",]$Cholesterol)

# Colesterol

which_row <- which(sub_training$Cholesterol==0)

sub_training_nozero <- sub_training[-which_row,]

for (i in which_row) {
  sub_training[i,]$Cholesterol <- median(sub_training_nozero$Cholesterol)
}

which(sub_training$Cholesterol==0)

# RestingBP

which(sub_training$RestingBP==0) # 266
sub_training <- sub_training[-266,]

sub_training_original <- sub_training_original[-266,]


### ANALISI CORRELAZIONE E DISTRIBUZIONI MARGINALI

# Calcoliamo la correlazione x verificare effetti di multicollinearita' tra gli input 
# Solo per le var quantitative
correlazione <- cor(sub_training[,c(1,4,5,8,10)])
round(correlazione,2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlazione, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
# Le variabili (quantitative) non presentano correlazioni elevate, 
# di conseguenza decidiamo di tenerle tutte nel nostro modello


### VERIFICA DELLA PRESENZA DI VALORI ANOMALI


par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  boxplot(sub_training[,i],main=colnames(sub_training)[i])
}

par(mfrow=c(1,1))

boxplot(sub_training)
sub_training %>% 
  ggplot() +
  geom_boxplot(aes(y =RestingBP),outlier.colour = "darkorange",fill="#0095B6") +
  theme_minimal() 
sub_training %>% 
  ggplot() +
  geom_boxplot(aes(y =Cholesterol),outlier.colour = "darkorange",fill="#E52B50") +
  theme_minimal() 
sub_training %>% 
  ggplot() +
  geom_boxplot(aes(y =Oldpeak),outlier.colour = "darkorange",fill="#3CB371") +
  theme_minimal() 



par(mfrow=c(1,2))
boxplot(sub_training$Cholesterol, main="Cholesterol")
hist(sub_training$Cholesterol, main="Cholesterol")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(sub_training$RestingBP, main="RestingBP")
hist(sub_training$RestingBP, main="RestingBP")
par(mfrow=c(1,1))

# Dall'analisi dei boxplot notiamo che Colesterol, RestingBP e Oldpeak presentano
# più valori anomali.
# Decidiamo di trasfromare con il logaritmo Colesterol e RestingBP, essendo entrambe positive.

sub_training$Cholesterol <- log(sub_training$Cholesterol)
sub_training$RestingBP <- log(sub_training$RestingBP)

par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  boxplot(sub_training[,i],main=colnames(sub_training)[i])
}

par(mfrow=c(1,2))
boxplot(sub_training$Cholesterol, main="Cholesterol")
hist(sub_training$Cholesterol, main="Cholesterol")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(sub_training$RestingBP, main="RestingBP")
hist(sub_training$RestingBP, main="RestingBP")
par(mfrow=c(1,1))

summary(sub_training)


### STANDARDIZZAZIONE
# Le variabili hanno intervalli di variazione diversi tra di loro -> vogliamo renderle 
# confrontabili 

# Variabile di supporto
matrix_indicators <- matrix(0, nrow=(dim(sub_training)[2]-1), ncol=2) 
colnames(matrix_indicators) <- c("median", "mad")

# Calcolo media e dev. st.
for (i in c(1,4,5,8,10)){
  matrix_indicators[i, "median"] <- median(sub_training[,i])
  matrix_indicators[i, "mad"] <- mad(sub_training[,i])
}

# Standardizzazione
for (i in c(1,4,5,8,10)){
  sub_training[, i] <- (sub_training[, i] - matrix_indicators[i, "median"])/matrix_indicators[i, "mad"]
}

par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  boxplot(sub_training[,i],main=colnames(sub_training)[i])
}

par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  hist(sub_training[,i],main=colnames(sub_training)[i])
}


############ FASE 3. VERIFICA DELLE ASSUNZIONI ##################
# Verifica delle assunzioni per l'applicabilita' della LDA e QDA
# Solo per le var quantitative continue

### 1. NORMALITA'
# Verifichiamo la normalita' condizionata alla classe

# Classe: 1 

variables <- colnames(sub_training)[c(1,4,5,8,10)]

par(mfrow = c(3, 2))
for(i in variables) {
  qqnorm(sub_training[sub_training$HeartDisease == 1, i], main = i); qqline(sub_training[sub_training$HeartDisease == 1, i], col = 2)
}

# Classe: 0

par(mfrow = c(3, 2))
for(i in variables) {
  qqnorm(sub_training[sub_training$HeartDisease == 0, i], main = i); qqline(sub_training[sub_training$HeartDisease == 0, i], col = 2)
}


# Per conferma della non normalità è necessario quindi un test di normalita': 
# test di Shapiro Wilk

# Variabili di supporto
pvalue_shapiro <- matrix(0, nrow = 5, ncol = 2)
rownames(pvalue_shapiro) = colnames(sub_training)[c(1,4,5,8,10)]
colnames(pvalue_shapiro) = c("HeartDisease", "Normal")

# Test Shapiro e costruzione di una matrice riassuntiva con i p-value condizionati alla classe
for (i in colnames(sub_training)[c(1,4,5,8,10)]){
  pvalue_shapiro[i, "HeartDisease"] <- shapiro.test(sub_training[sub_training$HeartDisease == 1, i])$p.value
  pvalue_shapiro[i, "Normal"] <- shapiro.test(sub_training[sub_training$HeartDisease == 0, i])$p.value
}
round(pvalue_shapiro, 5)

# i p-value portano a rifiutare l'ipotesi nulla di normalita' per la maggioranza delle variabili
# Non possiamo applicare LDA e QDA



############# FASE 4. CLASSIFICAZIONE #################

### MODELLI - TRAINING

### REGRESSIONE LOGISTICA

# Modello di regressione logistica e applicazione di una stepwise selection
model_logit <- glm(HeartDisease ~., data = sub_training, family = binomial)
summary(model_logit) # AIC = 333.72

step.model <- stepAIC(model_logit, direction = "both", trace = FALSE)
summary(step.model) # AIC = 324.83

# Il modello contiene le variabili: Age, Sex, ChestPainType, FastingBS,
# Oldpeak, ST_Slope

# Testiamo ora la presenza di punti influenti che potrebbero influenzare il nostro modello.

# Analisi dei punti influenti
par(mfrow=c(1,1))
influencePlot(step.model)

which(row.names(sub_training)==376) #16
which(row.names(sub_training)==786) #29
which(row.names(sub_training)==703) #133
which(row.names(sub_training)==323) #163
which(row.names(sub_training)==563) #219
which(row.names(sub_training)==326) #269

sub_training_pt <- sub_training[-c(16,29,133,163,219,269),]

model_logit2 <- glm(HeartDisease ~., data = sub_training_pt, family = binomial)
summary(model_logit2) # AIC = 307.01
step.model <- stepAIC(model_logit2, direction = "both", trace = FALSE)
summary(step.model) # AIC = 297.34
# = modello migliore con AIC piu' basso

# Il modello contiene le variabili: Age, Sex, ChestPainType, Cholesterol, FastingBS,
# Oldpeak, ST_Slope


# Verifichiamo ora se le variabili esplicative hanno una relazione lineare con log(p1/(1-p1)).

# Calcolo fitted values
probabilities <- predict(step.model, type = "response")
predictors <- c("Age","Oldpeak")

# Costruzione delle log(p1/(1-p1))
supp <- sub_training_pt[, c(1, 10)]
supp <- supp %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# Costruzione dei grafici
ggplot(supp, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(y="predictor value")



### KNN
# Nella fase successiva


### VALUTAZIONE CAPACITA' DI CLASSIFICAZIONE
### VALIDATION

# Prima di tutto dobbiamo applicare le diverse operazioni di cleaning e 
# sistemazione dei dati decise durante il training.

# Missing values

# Colesterol

which_row2 <- which(validation$Cholesterol==0)

for (i in which_row2) {
  validation[i,]$Cholesterol <- median(sub_training_nozero$Cholesterol)
}

which(validation$Cholesterol==0)

# RestingBP

which(validation$RestingBP==0) # 0


# Trasformazione logaritmica

validation$Cholesterol <- log(validation$Cholesterol)
validation$RestingBP <- log(validation$RestingBP)

# Standardizzazione
for (i in c(1,4,5,8,10)){
  validation[, i] <- (validation[, i] - matrix_indicators[i, "median"])/matrix_indicators[i, "mad"]
}


# KNN
# Possiamo utilizzare tutte le variabili numeriche e le qualitative non sconnesse, ovvero
# ChestPainType, ST_Slope

# Definizione dei valori di K da testare
K <- seq(1:100)
accuracy_knn_models <- NULL

sub_training_k <- sub_training[c(1,3,4,5,8,10,11,12)]
validation_k <- validation[c(1,3,4,5,8,10,11,12)]
# ci servono numeriche per il calcolo delle distanze 
sub_training_k$ChestPainType <- as.numeric(sub_training_k$ChestPainType)
sub_training_k$ST_Slope <- as.numeric(sub_training_k$ST_Slope)
validation_k$ChestPainType <- as.numeric(validation_k$ChestPainType)
validation_k$ST_Slope <- as.numeric(validation_k$ST_Slope)

# Calcolo accuratezza per ogni valore di K
for (k in K){
  model <- knn(sub_training_k[, -8], validation_k[, -8], cl=sub_training_k[, 8], k=k)
  accuracy <-   confusionMatrix(factor(validation_k[, 8]), model)$overall["Accuracy"]
  accuracy_knn_models <- c(accuracy_knn_models, accuracy)
}  

plot(K, accuracy_knn_models, type = "l")

which.max(accuracy_knn_models) # k = 55
max(accuracy_knn_models) # 0.844358

# Modello conclusivo KNN con K = 55
model <- knn(sub_training_k[, -8], validation_k[, -8], cl=sub_training_k[, 8], k=55, prob=TRUE)
confusionMatrix(factor(validation_k[, 8]), model)
# Accuracy = 0.8444
# Sensitivity = 0.8394 
# Specificity = 0.8500

# Regressione logistica - calcolo delle probabilità a posteriori
pred_logit <- predict(step.model, validation[, -12], type = "response")
# Trasformazioni probabilità in classe con soglia 0.5
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
confusionMatrix(factor(validation[, 12]), factor(as.vector(pred_logit_class)))
# Accuracy = 0.8249  
# Sensitivity = 0.8188 
# Specificity = 0.8319
?confusionMatrix

# Valutiamo ora le ROC curves e calcoliamo l’AUC.

pred_roclogit <- prediction(pred_logit, validation[, 12])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit # 0.8935545

prob_knn <- attributes(model)$prob
prob_knn <- 2*ifelse(model == "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, validation[, 12])
perf_knn<- performance(pred_rocknn,"tpr","fpr")
auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn # 0.8907349

par(mfrow = c(1, 2))
plot(perf_logit, colorize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "55-NN")


### MODELLI - TRAINING + VALIDATION

# Training finale
training <- rbind(sub_training_original, validation_original)
summary(training)

# Missing

# Colesterol

which_row3 <- which(training$Cholesterol==0)

training_nozero <- training[-which_row3,]

for (i in which_row3) {
  training[i,]$Cholesterol <- median(training_nozero$Cholesterol)
}

which(training$Cholesterol==0)

# RestingBP

which(training$RestingBP==0) # gia' eliminata in precedenza

# Trasformazione logaritmica

training$Cholesterol <- log(training$Cholesterol)
training$RestingBP <- log(training$RestingBP)

# Standardizzazione
matrix_indicators <- matrix(0, nrow=(dim(training)[2]-1), ncol=2) 
colnames(matrix_indicators) <- c("median", "mad")
for (i in c(1,4,5,8,10)){
  matrix_indicators[i, "median"] <- median(training[,i])
  matrix_indicators[i, "mad"] <- mad(training[,i])
}

for (i in c(1,4,5,8,10)){
  training[, i] <- (training[, i] - matrix_indicators[i, "median"])/matrix_indicators[i, "mad"]
}

# Modello logistico
model_logit_final <- glm(HeartDisease ~ Age+Sex+ChestPainType+FastingBS+Oldpeak+ST_Slope, family = binomial, data = training)
summary(model_logit_final) # AIC = 527.12


# Individuazione valori anomali
par(mfrow=c(1,1))
influencePlot(step.model)

which(row.names(training)==821) #5
which(row.names(training)==397) #50
which(row.names(training)==680) #64
which(row.names(training)==884) #173
which(row.names(training)==316) #399

# Valori influenti: 
training_pt <- training[-c(5,50,64,173,399), ]

# Modello finale
model_logit_final <- glm(HeartDisease ~ Age+Sex+ChestPainType+FastingBS+Oldpeak+ST_Slope, family = binomial, data = training_pt)
summary(model_logit_final) # AIC = 510.89

# L’algoritmo KNN possiamo valutarlo direttamente nella fase successiva.


### MODELLI - TEST

# Sistemiamo il test come da analisi.

# Missing

# Colesterol

which_row4 <- which(test$Cholesterol==0)

for (i in which_row4) {
  test[i,]$Cholesterol <- median(training_nozero$Cholesterol)
}

which(test$Cholesterol==0)

# RestingBP

which(test$RestingBP==0) # 0

# Trasformazione logaritmica

test$Cholesterol <- log(test$Cholesterol)
test$RestingBP <- log(test$RestingBP)


# Standardizzazione
for (i in c(1,4,5,8,10)){
  test[, i] <- (test[, i] - matrix_indicators[i, "median"])/matrix_indicators[i, "mad"]
}

# Regressione logistica
pred_logit <- predict(model_logit_final, test[, -12], type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
confusionMatrix(factor(test[, 12]), factor(as.vector(pred_logit_class)))
# accuracy 0.8913   
# Sensitivity 0.8824 
# Specificity 0.9024

# KNN

training_k <- training[c(1,3,4,5,8,10,11,12)]
training_k$ChestPainType <- as.numeric(training_k$ChestPainType)
training_k$ST_Slope <- as.numeric(training_k$ST_Slope)
test_k <- test[c(1,3,4,5,8,10,11,12)]
test_k$ChestPainType <- as.numeric(test_k$ChestPainType)
test_k$ST_Slope <- as.numeric(test_k$ST_Slope)

model <- knn(training_k[, -8], test_k[, -8], cl=training_k[, 8], k=55, prob=TRUE)
confusionMatrix(factor(test_k[, 8]), model)
# Accuracy = 0.7935
# Sensitivity = 0.8000 
# Specificity = 0.7857


pred_roclogit <- prediction(pred_logit, test_k[, 8])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit # 0.9286308

prob_knn <- attributes(model)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn <- 2*ifelse(model == "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, test_knn[, 22])
perf_knn<- performance(pred_rocknn,"tpr","fpr")
auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn # 0.8907349

par(mfrow = c(1, 2))
plot(perf_logit, colorize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "55-NN")



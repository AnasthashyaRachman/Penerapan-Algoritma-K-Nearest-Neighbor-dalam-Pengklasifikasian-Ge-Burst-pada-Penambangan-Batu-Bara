# Library yang dibutuhkan
library(readr)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(scales)

# Import Data 
seismic = read_csv("C:/Users/HP/Downloads/seismic-bumps.csv")
head(seismic)


# Eksplorasi Data
str(seismic)

summary(seismic)

sum(is.na(seismic))


# Preprocessing Data
## Menghapus kolom yang tidak diperlukan
seismic <- seismic[, -c(1, 2, 3, 8)]

## Mengubah nilai pada kolom class menjadi faktor
seismic$class <- as.factor(seismic$class)

## Normalisasi data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

seismic_norm <- as.data.frame(lapply(seismic[,-ncol(seismic)], normalize))
seismic_norm$class <- seismic$class

## Membagi data menjadi data pelatihan dan data uji dengan rasio 70:30
set.seed(123)
train.index <- createDataPartition(seismic$class, p = 0.7, list = FALSE)
train.data <- seismic[train.index, ]
test.data <- seismic[-train.index, ]

## Memisahkan fitur dan target dari data pelatihan dan data uji
train.features <- train.data[, -15]
train.target <- train.data$class
test.features <- test.data[, -15]
test.target <- test.data$class

# Pemodelan KNN
## Mencari nilai k terbaik dengan kurva elbow
accuracy_values <- data.frame(k = integer(), accuracy = numeric()) # Membuat dataframe kosong untuk menampung hasil akurasi
for (i in 1:20) {
  model_knn <- knn(train = train.features, 
                   test = test.features, 
                   cl = train.target, 
                   k = i)
  prediction_knn <- as.factor(model_knn)
  accuracy_values[i, 1] <- i
  accuracy_values[i, 2] <- sum(prediction_knn == test.target)/nrow(test.data)
}

## Membuat plot Elbow Curve
ggplot(accuracy_values, aes(x = k, y = accuracy)) +
  geom_line(color = "skyblue") +
  geom_point(color = "purple") +
  xlab("K") + ylab("Accuracy") +
  ggtitle("Elbow Curve for KNN Model") +
  theme(plot.title=element_text(face='bold',color='black',hjust=0.7,size=12))

k_best <- which.max(accuracy_values$accuracy)
cat("K terbaik:", k_best, "\n")
cat("Akurasi tertinggi:", max(accuracy_values$accuracy), "\n")

## Membuat model KNN dengan nilai k terbaik
model_knn_final <- knn(train = train.features, 
                       test = test.features, 
                       cl = train.target, 
                       k = k_best)


# Evaluasi Model
## Memprediksi data uji dengan model KNN yang telah dilatih
prediction_knn <- as.factor(model_knn_final)

## Menampilkan hasil prediksi pada data uji
confusion_matrix <- confusionMatrix(prediction_knn, test.target)
confusion_matrix

## Menghitung akurasi dan test error
accuracy.knn <- round(mean(test.target == prediction_knn), digits = 2)*100
print(paste('Akurasi model: ',accuracy.knn,"%"))

print(paste('Test error model: ',100-accuracy.knn,"%"))

# Visualisasi
## Distribusi variabel respons
ggplot(data = seismic, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_manual(values = c("skyblue", "darkorange")) +
  labs(title = "Distribusi Variabel Respons", x = "Class", y = "Jumlah") +
  theme_minimal()

## Density Plot
ggplot(seismic_norm, aes(x = energy, fill = class)) +
  geom_density(alpha = 0.5) +
  ggtitle("Distribusi Variabel Energy Berdasarkan Kelas") +
  theme(plot.title=element_text(face='bold',color='black',hjust=0.7,size=12))

## Violin Plot
ggplot(data = seismic, aes(x = class, y = energy, fill = class)) +
  geom_violin() +
  scale_fill_manual(values = c("skyblue", "darkorange")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Distribusi Energi Berdasarkan Class", 
       x = "Class", y = "Energi")

## Scatter Plot
ggplot(data = seismic, aes(x = genergy, y = gpuls, color = class)) +
  geom_point() +
  scale_color_manual(values = c("skyblue", "darkorange")) +
  scale_x_continuous(labels = comma, name = "Genergy") +
  scale_y_continuous(labels = comma, name = "Gpuls") +
  labs(title = "Distribusi Genergy dan Gpuls berdasarkan Class")

## Histogram
ggplot(data = seismic, aes(x = energy, fill = class)) +
  geom_histogram(bins = 20, alpha = 0.5) +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c("skyblue", "darkorange")) +
  labs(title = "Distibusi Energi berdasarkan Class", 
       x = "Energi", y = "Frekuensi")

## Boxplot
ggplot(seismic, aes(x = class, y = genergy, fill = class)) + 
  geom_boxplot() +
  ggtitle("Boxplot Energi terhadap Class") +
  scale_y_continuous(labels = scales::comma)

## Polar plot
ggplot(data = seismic, aes(x = class, y = energy, color = class)) +
  geom_point() + scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("skyblue", "darkorange")) +
  labs(title = "Polar Plot Energi terhadap Class", 
       x = "Class", y = "Energy")

# Distibusi Atribut Gpuls
ggplot(seismic, aes(x = gpuls, group = 1)) +
  stat_count() +
  labs(title = "Distibusi Atribut Gpuls", x = "gpuls", y = "Nilai") +
  theme_minimal()

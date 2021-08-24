#import library
library(keras)
library(tensorflow)
library(scales)
library(dplyr)

#Load train data
train = read.csv("./data/Google_Stock_Price_Train.csv")

#Normalize data
scale = c(mean(train$Open),sd(train$Open))
y = (train$Open - scale[1])/scale[2]
y = data.matrix(y)

#Create data structure
x_train = t(sapply(1:(length(y)-60), 
                   function(x) y[x:(x+60-1),1]))
x_train = array(data = as.numeric(unlist(x_train)),
                dim = c(nrow(x_train),ncol(x_train),1))

y_train = t(sapply((1 + 60):(length(y)),
                         function(x) y[x]))
y_train = array(data = as.numeric(unlist(y_train)))


#Build LSTM model
LSTM = keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape =c(60,1)) %>%
        layer_dropout(rate = 0.2) %>%
          layer_lstm(units = 50, return_sequences = TRUE) %>%
            layer_dropout(rate=0.2) %>%
              layer_lstm(units = 50, return_sequences = TRUE)%>%
                layer_dropout(rate=0.2)%>%
                  layer_lstm(units = 50)%>%
                    layer_dropout(rate=0.2) %>%
                      layer_dense(units = 1)
compile(LSTM,optimizer = 'adam',loss='mean_squared_error')
LSTM %>% fit(x_train,y_train,epochs = 100,batch_size = 32)


#import text data
test = read.csv("./data/Google_Stock_Price_Test.csv")
real_stock_price = test[2]
total = rbind(train[2],test[2])
total = (total-scale[1])/scale[2]
inputs = total[(lengths(total)-lengths(test[2])-60+1):lengths(total),]
inputs = data.matrix(inputs)
x_test = t(sapply(1:(length(inputs)-60), 
                   function(x) inputs[x:(x+60-1),1]))
x_test = array(data = as.numeric(unlist(x_test)),
                dim = c(nrow(x_test),ncol(x_test),1))


#visualize the prediction

predict_stock_price = predict(LSTM,x_test,batch_size = 32)
predict_stock_price = predict_stock_price*scale[2]+scale[1]
plot(real_stock_price$Open,type='l',col="blue")
lines(predict_stock_price,col="red")

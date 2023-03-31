library(tensorflow)
library(keras)
library(dplyr)
library(ggplot2)
library(viridis)
data = readRDS('/CNNdatasetfv3.rds')
data$x = data$x[c(1:3000),,,]
data$x = round(data$x,2)
index = sample(3000,600,replace = F)
test = list(data$x[index,,,])
train = list(data$x[-index,,,])
names(test) = 'x'
names(train) = 'x'
train$y = data$z[-index,,]
test$y = data$z[index,,]

metric_coeff_determination = custom_metric("coeff_determination", 
                                           function(y_true, y_pred) 
                                           {
                                             SS_res =  k_sum(k_square( y_true - y_pred ))
                                             SS_tot = k_sum(k_square( y_true - k_mean(y_true) ) )
                                             return ( 1 - SS_res/(SS_tot + k_epsilon()) )
                                           }
)

decay = function(epoch, lr) {
  if (epoch < 400) 1e-3
  else if (epoch >= 400 && epoch < 600) 1e-4
  else 1e-5
}


model = keras_model_sequential() %>% 
  layer_conv_2d(filters = 128,kernel_size = c(3,3),activation = "relu", 
                input_shape = c(31,6,3),padding = 'same',
                kernel_regularizer = regularizer_l2(l=0.001)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 256, kernel_size = c(3,3), 
                activation = "relu",kernel_regularizer = regularizer_l2(l=0.001),
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2,2))



model %>% 
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  #layer_dropout(0.1)%>%
  layer_dense(units = 512, activation = "relu") %>%
  #layer_dropout(0.1)%>%
  layer_dense(units = 256, activation = "relu") %>%
  #layer_dropout(0.1)%>%
  layer_dense(units = 336, activation = "linear")

model %>% compile(
  optimizer = 'adam',
  loss = 'mean_absolute_error',
  metrics = 'mean_squared_error'
)

history = model %>% 
  fit(
    x = train$x, y = train$y,
    epochs =700,
    validation_data = unname(test),
    verbose = 2,
    callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 20),
                     callback_learning_rate_scheduler(decay))
  )
plot(history)+theme_classic()
evaluate(model, test$x, test$y, verbose = 0)


model %>% save_model_hdf5("my_model.h5")
new_model = load_model_hdf5("my_model.h5")

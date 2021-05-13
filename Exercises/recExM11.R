
## Problem 2
# a) One hidden layer and bias in input- as well as hidden layer. ReLU activation. 5 node in hidden layer and 10 in input (excluding bias).
# Number of params:
11*5 + 6*1

# b) 4 node in input layer (no bias). 2 hidden layers: First has 10 nodes + bias and Relu. Secon has 5 node + bias and Relu. Output is of dim 1 and has sigmoid.
# Number of params:
4 * 10 + 11 * 5 + (5 + 1)

## Problem 3

library(ElemStatLearn)

train_data=zip.train[,-1]
train_labels=factor(zip.train[,1])
test_data=zip.test[,-1]
test_labels=factor(zip.test[,1])

means <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
test_data <- scale(test_data, center = means, scale = std)
train_data <- scale(train_data, center = means, scale = std)

library(nnet)
NN <- nnet(train_labels~., data=train_data, size=5, MaxNWts=3000, maxit=5000)
summary(NN)
pred <- predict(NN, newdata=test_data, type="class")
library(caret)
confusionMatrix(factor(pred),test_labels)

## Problem 4
# a)

# Install the keras R package 
install.packages("keras")
# Install the core Keras library + TensorFlow
library(keras)
install_keras()
# for machines with NVIDIA GPU
# install_keras(tensorflow = "gpu")

# Data Preprocessing:
library(keras)
mnist = dataset_mnist()
x_train = mnist$train$x
y_train = mnist$train$y
x_test = mnist$test$x
y_test = mnist$test$y


# reshape
x_train = array_reshape(x_train, c(nrow(x_train), 28*28))
x_test = array_reshape(x_test, c(nrow(x_test), 28*28))
# rescale
x_train = x_train / 255
x_test = x_test / 255

y_train = to_categorical(y_train, 10)
y_test = to_categorical(y_test, 10)
dim(x_test)

model = keras_model_sequential() %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(28*28)) %>% 
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy", metrics = c("accuracy")
)

history = model %>% fit(x_train, y_train,
                        epochs = 20,
                        batch_size = 128,
                        validation_split = 0.2)

str(history)
plot(history)
model %>% evaluate(x_test,y_test)
str(model)

# b)

model.2 = keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = c(28*28)) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

model.2 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy", metrics = c("accuracy")
)

history.2 = model.2 %>% fit(x_train, y_train,
                        epochs = 20,
                        batch_size = 128,
                        validation_split = 0.2)
plot(history.2)

model.2 %>% evaluate(x_test,y_test)

# c)

model.reg <- keras_model_sequential() %>% 
     layer_dense(units = 128, activation = "relu", input_shape = c(28*28),
              kernel_regularizer = regularizer_l2(l = 0.01)) %>% 
  layer_dropout(0.6) %>% layer_dense(units = 128, activation = "relu", 
              kernel_regularizer = regularizer_l2(l = 0.01)) %>% 
  layer_dense(units = 10, activation = "softmax")

model.reg %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy", metrics = c("accuracy")
)

history.reg <- model.reg %>% fit(x_train, y_train, 
                   epochs = 20,
                   batch_size = 128,
                   validation_split = 0.2)
plot(history.reg)
model.reg %>% evaluate(x_test,y_test)






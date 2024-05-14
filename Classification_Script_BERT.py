from google.colab import drive
drive.mount('/content/drive')
!ls /content/drive/

import pandas as pd

file_path = '/content/drive/MyDrive/hicks_personal_training_06292023.csv'
df = pd.read_csv(file_path, encoding = "ISO-8859-1")


df.head(500)

import numpy as np
import matplotlib.pyplot as plt
plt.style.use('ggplot')


num_classes = len(df["hicks_personal"].value_counts())

colors = plt.cm.Dark2(np.linspace(0, 1, num_classes))
iter_color = iter(colors)

df['hicks_personal'].value_counts().plot.barh(title="DASH Stories (n, %)",
                                                 ylabel="Hicks Personal Categories",
                                                 color=colors,
                                                 figsize=(9,9))

for i, v in enumerate(df['hicks_personal'].value_counts()):
  c = next(iter_color)
  plt.text(v, i,
           " "+str(v)+", "+str(round(v*100/df.shape[0],2))+"%",
           color=c,
           va='center',
           fontweight='bold')

# map topic descriptions to labels
df['Labels'] = df['hicks_personal'].map({ 'retired': 0,
                                          'serious leisure': 1,
                                          'personal':2,
                                          'chronic illness':3,
                                          'family':4,
                                          'Job Transition':5,
                                          'improve skills':6,
                                          'veteran':7

                                   })

# drop unused column
df = df.drop(['hicks_personal'], axis=1)



df.head(50)

df['Text'] = df['story']

df.head(1000)

len(df)

df = df.dropna() # dropped 3 observations (1596 to 1593)

import tensorflow as tf

#print("Tensorflow version " + tf.__version__)

#try:
#  tpu = tf.distribute.cluster_resolver.TPUClusterResolver()  # TPU detection
#  print('Running on TPU ', tpu.cluster_spec().as_dict()['worker'])
#except ValueError:
#  raise BaseException('ERROR: Not connected to a TPU runtime; please see the previous cell in this notebook for instructions!')

#tf.config.experimental_connect_to_cluster(tpu)
#tf.tpu.experimental.initialize_tpu_system(tpu)
#tpu_strategy = tf.distribute.TPUStrategy(tpu)


from sklearn.model_selection import train_test_split

# from tensorflow.keras import mixed_precision
# mixed_precision.set_global_policy('mixed_float16')

y = tf.keras.utils.to_categorical(df["Labels"].values, num_classes=num_classes)

x_train, x_test, y_train, y_test = train_test_split(df['story'], y, test_size=0.20)

!pip install tensorflow_text
import tensorflow_hub as hub
import tensorflow_text as text

preprocessor = hub.KerasLayer("https://tfhub.dev/google/universal-sentence-encoder-cmlm/multilingual-preprocess/2")
encoder = hub.KerasLayer("https://tfhub.dev/google/universal-sentence-encoder-cmlm/multilingual-base/1")


def get_embeddings(sentences):
  '''return BERT-like embeddings of input text
  Args:
    - sentences: list of strings
  Output:
    - BERT-like embeddings: tf.Tensor of shape=(len(sentences), 768)
  '''
  preprocessed_text = preprocessor(sentences)
  return encoder(preprocessed_text)['pooled_output']


#get_embeddings([
#    "I am using dash for a personal project."]
#)

from keras import backend as K

def balanced_recall(y_true, y_pred):
    """This function calculates the balanced recall metric
    recall = TP / (TP + FN)
    """
    recall_by_class = 0
    # iterate over each predicted class to get class-specific metric
    for i in range(y_pred.shape[1]):
        y_pred_class = y_pred[:, i]
        y_true_class = y_true[:, i]
        true_positives = K.sum(K.round(K.clip(y_true_class * y_pred_class, 0, 1)))
        possible_positives = K.sum(K.round(K.clip(y_true_class, 0, 1)))
        recall = true_positives / (possible_positives + K.epsilon())
        recall_by_class = recall_by_class + recall
    return recall_by_class / y_pred.shape[1]

def balanced_precision(y_true, y_pred):
    """This function calculates the balanced precision metric
    precision = TP / (TP + FP)
    """
    precision_by_class = 0
    # iterate over each predicted class to get class-specific metric
    for i in range(y_pred.shape[1]):
        y_pred_class = y_pred[:, i]
        y_true_class = y_true[:, i]
        true_positives = K.sum(K.round(K.clip(y_true_class * y_pred_class, 0, 1)))
        predicted_positives = K.sum(K.round(K.clip(y_pred_class, 0, 1)))
        precision = true_positives / (predicted_positives + K.epsilon())
        precision_by_class = precision_by_class + precision
    # return average balanced metric for each class
    return precision_by_class / y_pred.shape[1]

def balanced_f1_score(y_true, y_pred):
    """This function calculates the F1 score metric"""
    precision = balanced_precision(y_true, y_pred)
    recall = balanced_recall(y_true, y_pred)
    return 2 * ((precision * recall) / (precision + recall + K.epsilon()))

i = tf.keras.layers.Input(shape=(), dtype=tf.string, name='text')
x = preprocessor(i)
x = encoder(x)
x = tf.keras.layers.Dropout(0.2, name="dropout")(x['pooled_output'])
x = tf.keras.layers.Dense(num_classes, activation='softmax', name="output")(x)

model = tf.keras.Model(i, x)

n_epochs = 20

METRICS = [
      tf.keras.metrics.CategoricalAccuracy(name="accuracy"),
      balanced_recall,
      balanced_precision,
      balanced_f1_score
]

earlystop_callback = tf.keras.callbacks.EarlyStopping(monitor = "val_loss",
                                                      patience = 3,
                                                      restore_best_weights = True)

### code ###
activation_function = 'relu'
batch_size = 32
learning_rate = 0.001
regularization = tf.keras.regularizers.l2(0.01)

model.compile(optimizer=tf.keras.optimizers.AdamW(learning_rate=learning_rate),
              loss="categorical_crossentropy",
              metrics=METRICS)

###

#model.compile(optimizer = "adam",
#              loss = "categorical_crossentropy",
#              metrics = METRICS)

model_fit = model.fit(x_train,
                      y_train,
                      epochs = n_epochs,
                      validation_data = (x_test, y_test),
                      callbacks = [earlystop_callback])

x = list(range(1, n_epochs+1))
metric_list = list(model_fit.history.keys())
num_metrics = int(len(metric_list)/2)

fig, ax = plt.subplots(nrows=1, ncols=num_metrics, figsize=(30, 5))

for i in range(0, num_metrics):
  ax[i].plot(x, model_fit.history[metric_list[i]], marker="o", label=metric_list[i].replace("_", " "))
  ax[i].plot(x, model_fit.history[metric_list[i+num_metrics]], marker="o", label=metric_list[i+num_metrics].replace("_", " "))
  ax[i].set_xlabel("epochs",fontsize=14)
  ax[i].set_title(metric_list[i].replace("_", " "),fontsize=20)
  ax[i].legend(loc="lower left")

def predict_class(reviews):
  '''predict class of input text
  Args:
    - reviews (list of strings)
  Output:
    - class (list of int)
  '''
  return [np.argmax(pred) for pred in model.predict(reviews)]


# predict_class(reviews)


reviews = [ 
    
         ## ADD COMMENTS TO BE CLASSIFIED HERE ##  
]

predict_class(reviews)

elsabry_mid_cat_unclassified = predict_class(reviews)

remaining_df = pd.DataFrame(elsabry_mid_cat_unclassified)

# write the DataFrame to a CSV file
remaining_df.to_csv('hicks_PERSONAL_06292023.csv')

import os
import itertools
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
from sklearn.metrics import confusion_matrix
from tensorflow.keras.models import Sequential
from keras.preprocessing import image
from tensorflow.keras.preprocessing.image import ImageDataGenerator, array_to_img, img_to_array
from tensorflow.keras.applications.vgg16 import VGG16
from keras import backend as K

# import keras_tuner as kt

train_label_list = os.listdir("C:/Users/s1000334/Documents/Python/ML/train_small")
test_label_list = os.listdir("C:/Users/s1000334/Documents/Python/ML/test_small")

print(train_label_list)
print(len(train_label_list))

train_dir = r"C:/Users/s1000334/Documents/Python/ML/train_small/"
test_dir = r"C:/Users/s1000334/Documents/Python/ML/test_small/"

train_data_gen = ImageDataGenerator(rescale=1. / 255,
                                    validation_split=0.2,
                                    rotation_range=40,
                                    #width_shift_range=0.2,
                                    #height_shift_range=0.2,
                                    #shear_range=0.2,
                                    # zoom in from 50% to zoom out to 50%
                                    zoom_range=[0.5,1.5],
                                    horizontal_flip=True
                                    #vertical_flip=True
                                    # change the brightness from 20% darker to 20% lighter
                                    #brightness_range=[0.2,1.2]
                                    )

validation_data_gen = ImageDataGenerator(rescale=1. / 255)
test_data_gen = ImageDataGenerator(rescale=1. / 255)

TARGET_SIZE = (224,224)
BATCH_SIZE = 32
CLASS_MODE = 'categorical'
EPOCHS = 5

""" 
------------------------------------------------------------------
SET UP THE TRAINING, VALIDATION, AND TESTING GENERATORS
------------------------------------------------------------------
"""

train_generator = train_data_gen.flow_from_directory(directory=train_dir,
                                                     subset='training',
                                                     target_size=TARGET_SIZE,
                                                     color_mode="rgb",
                                                     class_mode=CLASS_MODE,
                                                     classes=train_label_list,
                                                     shuffle=True
                                                     )

validation_generator = train_data_gen.flow_from_directory(directory=train_dir,
                                                          subset='validation',
                                                          target_size=TARGET_SIZE,
                                                          color_mode="rgb",
                                                          class_mode=CLASS_MODE,
                                                          classes=train_label_list,
                                                          shuffle=True
                                                          )

test_generator = test_data_gen.flow_from_directory(directory=test_dir,
                                                   target_size=TARGET_SIZE,
                                                   batch_size=BATCH_SIZE,
                                                   class_mode=CLASS_MODE,
                                                   shuffle=False)

print(train_generator.class_indices)

""" 
------------------------------------------------------------------
SET UP THE MODEL
------------------------------------------------------------------
"""

VGG16_MODEL = VGG16(
    weights="imagenet",
    include_top=False
)

VGG16_MODEL.trainable = False  # freeze weights

VGG16_MODEL.summary()

#K.clear_session()

model = Sequential()
model.add(VGG16_MODEL)
model.add(tf.keras.layers.GlobalAveragePooling2D())
model.add(tf.keras.layers.Dense(1024, activation="relu"))
model.add(tf.keras.layers.Dropout(0.2))
model.add(tf.keras.layers.Dense(len(train_label_list), activation='softmax'))

model.summary()

""" 
------------------------------------------------------------------
COMPILE THE MODEL
------------------------------------------------------------------
"""

model.compile(loss=tf.keras.losses.categorical_crossentropy,
              optimizer=tf.keras.optimizers.Adam(learning_rate=0.001),
              metrics=['accuracy'])

history = model.fit(train_generator,
                    epochs=EPOCHS,
                    validation_data=validation_generator,
                    validation_steps=2
                    )

print(history.history.keys())

""" 
------------------------------------------------------------------
PLOT THE ACCURACY AND LOSS FOR THE TRAINING AND VALIDATION DATA
------------------------------------------------------------------
"""

# summarize history for accuracy
plt.plot(history.history['accuracy'])
plt.plot(history.history['val_accuracy'])
plt.title('model accuracy')
plt.ylabel('accuracy')
plt.xlabel('epoch')
plt.legend(['train', 'validation'], loc='upper left')
plt.savefig("accuracy.png", dpi=350)
plt.show()

# summarize history for loss
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('model loss')
plt.ylabel('loss')
plt.xlabel('epoch')
plt.legend(['train', 'validation'], loc='upper left')
plt.savefig("loss.png", dpi=350)
plt.show()

results = model.evaluate(test_generator)
print("test loss, test acc:", results)

""" 
------------------------------------------------------------------
SEE HOW WELL THE MODEL PERFORMS ON THE TEST DATA 
------------------------------------------------------------------
"""

#test_generator.reset()
predictions = model.predict(test_generator)
probs = predictions.max(1)

true_classes = test_generator.classes

predicted_classes = np.argmax(predictions, axis=1)

cm = confusion_matrix(true_classes, predicted_classes)

""" 
------------------------------------------------------------------
CONFUSION MATRIX
------------------------------------------------------------------
"""


def plot_confusion_matrix(cm, classes,
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    if normalize:
        cm = np.around(cm.astype('float') / cm.sum(axis=1)[:, np.newaxis], decimals=2)
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    print(cm)

    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, cm[i, j],
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    plt.tight_layout()
    plt.savefig("./confusion_matrix.png", dpi=350)
    plt.show()


conf_mat = plot_confusion_matrix(cm, classes=train_label_list, title="Confusion Matrix", normalize=True)

""" 
------------------------------------------------------------------
CONFUSION MATRIX ALTERNATIVE USING SEABORN
------------------------------------------------------------------
"""
import seaborn as sns

# Normalise
cmn = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
fig, ax = plt.subplots(figsize=(10, 10))
sns.heatmap(cmn, annot=True, fmt='.2f', xticklabels=train_label_list, yticklabels=train_label_list, cmap="viridis")
plt.ylabel('Actual')
plt.xlabel('Predicted')
plt.tight_layout()
plt.savefig("./confusion_matrix_seaborn.png", dpi=350)
plt.show()

""" 
------------------------------------------------------------------
PRINT PREDICTIONS 
------------------------------------------------------------------
"""

labels = (test_generator.class_indices)
labels = dict((v, k) for k, v in labels.items())
prediction_labels = [labels[k] for k in predicted_classes]
filenames = test_generator.filenames
results = pd.DataFrame({"Filenames": filenames,
                        "Predictions": prediction_labels,
                        # "Predicted class:":predicted_classes,
                        "Probabilities": probs
                        })

# To force the whole dataframe to be printed to the screen
with pd.option_context('display.max_rows', None, 'display.max_columns', None): print(results)

"""
Test one unseen image using the model
# load and resize image to 200x200
#test_image = image.load_img('C:/Users/s1000334/Documents/Python/ML/test_small/AFRICAN CROWNED CRANE/1.jpg', target_size=(32,32))
test_image = image.load_img('C:/Users/s1000334/Documents/Python/ML/unseen_images/east-african-crowned-crane-4.jpg', target_size=(32,32))
# convert image to numpy array
images = image.img_to_array(test_image)
# expand dimension of image
images = np.expand_dims(images, axis=0)
# making prediction with model
pred = model.predict(images)
print(pred)
pred_class = np.argmax(model.predict(images), axis=-1)
print(pred_class)
print(train_generator.class_indices)
"""

"""
------------------------------------------------------------------
Test a folder of unseen images using the model
------------------------------------------------------------------
"""
unseen_image_directory = r"C:/Users/s1000334/Documents/Python/ML/unseen_images"
unseen_image_filenames = []
unseen_image_class_prediction = []
unseen_image_probs = []

for filename in os.listdir(unseen_image_directory):
    unseen_image_filenames.append(filename)
    full_path = os.path.join(unseen_image_directory, filename)
    # load and pre-process the images
    unseen_image = image.load_img(full_path, target_size=(TARGET_SIZE))
    unseen_image = image.img_to_array(unseen_image)
    unseen_image = np.expand_dims(unseen_image, axis=0)
    # predict using the model
    unseen_prediction = model.predict(unseen_image)
    # get the class with the highest probability for the image in question
    unseen_pred_class = np.argmax(unseen_prediction)
    unseen_image_class_prediction.append(train_label_list[unseen_pred_class])
    unseen_image_probs.append(unseen_prediction.max(1))
    #print(unseen_pred)
    #print(unseen_pred_class)

print(train_generator.class_indices)

""" 
------------------------------------------------------------------
PRINT UNSEEN IMAGE PREDICTIONS 
------------------------------------------------------------------
"""

pred_labels = unseen_image_class_prediction

res = pd.DataFrame({"Filenames": unseen_image_filenames,
                    "Predictions": pred_labels,
                    "Probabilities": unseen_image_probs
                        })

print(res)
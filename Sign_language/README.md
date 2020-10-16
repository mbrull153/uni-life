# Sign language classifier and prediction text

THe idea behind this project is to develop an application to help people that need to use sign language to communicate more easily. It has two approaches, first the classification given a bunch of images that represent different letters in sign language, and secondly its based in predicting text. Given the output of the classification, we use it as the input for the text prediction to predict the full word given some letters.

**Classification problem**

The classification problem uses a CNN to classify the given image to the represent letter.

**Text generator problem**

The text generator used LSTM to predict given some input letter a full posible word.

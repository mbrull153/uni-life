# Image search engine

A search engine problem based on images, so the idea is that by using different image descriptor and different similarity metric, what searching output do we obtain, which combination is more accurate?

4 steps of ISE:

1. Defining your image descriptor: Decide what aspect of the image you want to describe, like color, texture, et...
2. Indexing your dataset: Apply this image descriptor to each image in your dataset, extract features from these images, and write the features to storage so that they can be later compared for similarity.
3. Defining your similarity metric: Metric of comparision (cosine distance, chi-squared distance, euclidean distance,...)
4. Searching: Perform an actual search

**First combination**
1. Image descriptor:  RGB, HSV
2. Similarity metrics: chi squared, manhattan distance, cosine similarity

**Second combination**
1. Image descriptor: HOG
2. Similarity metrics: chi squared, manhattan distance, cosine similarity

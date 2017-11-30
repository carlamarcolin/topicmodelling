# topicmodelling
Developments in R regarding Topic Modelling studies in PhD course.
- Function 1: JacSim: Jaccard similarity for text. Analyzes topic descriptors two by two. Parameters:
  - nWords = Numeric. Amount of words to be compared (suggested: 10).
  - nDim = Numeric. Amount of dimensions (vectors) to be considered.
  - matrix = Matrix. The Term-Topic matrix (suggested: tk from lsa package).
  - rotate = Logical. Indicate to apply matrix rotation, implemented with promax. For varimax, just perform a simple code editing. 
- Function 2: ordered.lsa: function that ordered Term-Topic matrix (contribuitor: Rodrigo Heldt). Parameters:
  - decomp.matrix = the Term-Topic matrix (suggested: tk from lsa package).
- Function 3: WebScrapping.R: WebScrapper to collect comments from TripAdvisor. Scrap comments considering:
  - List of main URL from each hotel.
  - List of sequence of comments (pages available x comments per page).
- Function 4: Classifiers: Trials with SVM and Naive Bayes classifiers in R
- Function 5: Neural Networks and Topic Modeling (NNandTM): Combine Neural Network Classifier with LSA for classify and construct main topics regarding TripAdvisor data.

# Presentations
  Presentations regarding the work-in-progress (Place, City, Date)
  - TripAdvisor_Classifier_: Structure for TripAdvisor classifier with SERVQUAL (Oxford Brookes University, Oxford, Nov/17)

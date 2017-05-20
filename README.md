# topicmodelling
Developments in R regarding Topic Modelling studies in PhD course.
- Function 1: JacSim: Jaccard similarity for text. Analyzes topic descriptors two by two. Parameters:
  - nWords = amount of words to be compared (suggested: 10).
  - nDim = amount of dimensions (vectors) to be considered. Numeric.
  - matrix = the Term-Topic matrix (suggested: tk from lsa package).
- Function 2: ordered.lsa: function that ordered Term-Topic matrix (by Rodrigo Heldt). Parameters:
  - decomp.matrix = the Term-Topic matrix (suggested: tk from lsa package).

# Set Similarity Join Algorithms (ALL & PPJ)

## Functionality
The code computes the similarity between two collection of sets (two different sets or self-join on a single dataset) and print the number of similar pair of sets. It uses ALL Pairs and PP Join algorithms. 

## How to use
- Place your dataset in `data` directory (Each line represents a set and the tokens of the set are separated by whitespace).
- Set the your desired `threshold` for similarity checking in `Main` (The code uses Jaccard Similarity).
- To switch between ALL and PPJ algorithms, just instantiate the proper class in `Main` (As explained in the comments).
- The default mode is self join. If you want to check similarity of two datasets, follow the comments in `Main`.
- Everything is set up! Now just run `sbt run` to see the number of similarity pairs.

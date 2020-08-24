
This project used Expectation-Maximization (EM) method to estimate parameters of Hidden Markov Model **without** using R packages:

1. initialize arbitrary parameters
2. use forward-backword method to find most probable path state
3. given path states find maximum likelihood of parameters 
4. update parameters and go to step 2 until estimation become stable

This project also forecasted future events using estimated parameters.

For more detail, please read **Report** file. 

🪁

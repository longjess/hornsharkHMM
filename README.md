# hornsharkHMM
Functions for Analyzing Horn Shark Data Using Hidden Markov Models (HMMs)

This package provides functions for simulating data from and fitting data to a variety of normal HMMs. The HMMs covered include univariate and multivariate normal HMMs, with and without autoregressive structures. For the multivariate HMMs, there are also functions that additionally assume independence between the variables. The functions cover the following features:
* Generating sample data from an HMM
* Estimation of HMM parameters from data using maximum likelihood estimation (MLE)
* Global decoding of states using the Viterbi algorithm, to find the most likely sequence of states given data and an HMM 
* Generating and plotting normal pseudo-residuals
* Bootstrapping estimates of HMM parameters, and using those estimates to estimate the covariance matrix and confidence intervals for the parameters

There are also functions for exploring accelerometer data on horn shark movement gathered from a lab environment. The data includes dynamic and static acceleration data, ODBA, timestamps, and behavior labels. There are functions to produce a variety of plots, including histographs, line graphs, ACF/PACF plots, and correlation plots. Futhermore, there are functions for plotting the mean and log SD of the acceleration data. 

Several of the functions were based on examples from the book "Hidden Markov Models for Time Series" by Iain L. MacDonald, Roland Langrock, and Walter Zucchini.

Notes:
* Currrently the functions for generating multivariate pseudo-residuals are inaccurate

# Senior-Thesis
"An Examination of Timeout Value, Strategy, and Momentum in NCAA Division 1 Men’s Basketball"

In Partial Fulfillment of the Requirements for the Degree Bachelor of Science in Applied Mathematics

Yale University

Luke Benz

April 2019

Advisor: Xiaofei Wang

---
__scrape.R:__ Pulls data using [`ncaahoopR`](https://github.com/lbenz730/ncaahoopR) package and stores it in __pbp_data/__ and __test_pbp_data/__. Due to GitHub only allowing 1000 files per directory, those directories have been broken down into smaller subdirectories containing 1000 files each. 

__clean.R:__ Cleans data. Due to GitHub size restrictions, the cleaned .rda objects used in subsequent analyses are unable to be uploaded. They can be recovered by running this script.

__fit_model.R:__ Fits various win probability models discussed in Chapter 3.

__model_cv.R:__ Predicts win probability models discussed in Chapter 3 on data in __test_pbp_data/__.

__eval.R:__ Evaluates various win probability models discussed in Chapter 3.

__score_runs.R:__ Computes net score differentials in intervals before and after each time stamp.

__pts_above_exp.R:__ Framework for building mixed-effects models to evaluate points above expectation after timeouts.

__imputation_evaluation.R:__ Evaluates lines imputed by [`ncaahoopR`](https://github.com/lbenz730/ncaahoopR) by comparing them to Vegas pointspreads. Imputed lines are available in __NCAA_Hoops_Results_2017_Final.csv__ (2016-17), __training.csv__(2017-18), and __2019_Final.csv__ (2018-19). Results are stored in __line_imputation.csv__.

__intro_graphics.R:__ Code used for figures and tables in the introduction.

__chap_2_graphics.R:__ Code used for figures and tables in Chapter 2.

__chap_3_graphics.R:__ Code used for figures and tables in Chapter 3.

__chap_4_graphics.R:__ Code used for figures and tables in Chapter 4.
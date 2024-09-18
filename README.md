# Don't Jump to Faulty Conclusions
R-code for the paper titled "Don’t Jump to Faulty Conclusions: Using the Synthetic Control Method to Evaluate the Effect of a Counterfactual Lockdown in Sweden"
Available at: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4959827

There are three codes:
* "010 Create Mortality_data.R"
* "020 Create WB_data.R"
* "100 SCM, lockdown and winter holidays.R"

First run 010 and 020 which prepares the data. This only need to be done once. Then run 100.

Code 100 runs several scenarios based on lopps running over various variables:
* dependents (age groups RTotal, R0_64, or R65p)
* predictor_list (full = mortality and WB-variables, single = only mortality)
* prewindow (yearweek = 201627, 201127, or 200727)
* swe_related_codes (different "versions" of Sweden.

In the main analysis I only use a selected group of these scenarios. To speed up the code, I limit the scenarios with the following code on line 502:
  if (!((pw %in% 1:3 & i == 1) | (pw == 1 & i %in% 4:7))) {
    next  # Skip the combination if it doesn't match the criteria
  }

  Remove this code line to get data and figures for all scenarios

  

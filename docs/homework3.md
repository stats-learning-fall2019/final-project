# Homework 3
### Group Members: James Willson, Kun Li, Sean Kugele

## I. Objectives and Activities

**Objective:** 
Our objective is to create statistical models from the [Global Terrorism Database](https://www.kaggle.com/START-UMD/gtd) data set for the purposes of prediction and inference. Specific research goal are outlined in the Section II, along with the principal data scientist assigned to each. Proposed solutions and methods of evaluation will differ depending on the nature of each question, and will be discussed individually in the context of each research topic.

**Data Set:**
The *Global Terrorism Database* contains over 180,000 observations and 135 variables. Each observation corresponds to a terrorism incident that occurred between 1970 and 2017. The type of information available for each incident is very diverse, including (but not limited to) the following:

1. *Date Variables* (Year, Month, Day, etc.)
2. *Geospatial Variables* (Lat/Long, Region, Country, City, Province, etc.)
3. *Incident Descriptive Variables* (Attack Type, Duration of Incident, Success/Failure, Weapons Used, Targets, etc.)
4. *Perpetrator Descriptive Variables* (Terrorist group membership, # Perpetrators, etc.)
5. *Casualty and Damage Variables* (# Fatalities, # Injured, etc.)  

The diversity of the variables in this data set lends itself to a variety of research directions, and supports the creation of numerous regression and categorical models.

## II. Research Questions and Proposed Approaches with Individual Assignments
Each sub-section below identifies the agenda and responsibilities of each data scientist. Each data scientist is expected to take ownership of their agenda, including implementation of solutions, final write-up, and presentation. Sean Kugele will serve as team lead, facilitating group collaboration and assisting in the resolution of any impediments that jeopardize the fulfillment of the research goals outlined below.

### James Willson (Data Scientist)
Goal 1: Predict if an attack will be successful based on a variety of different factors.

Goal 2: Estimate the number of casualties in a successful terrorist attack.

### Kun Li (Data Scientist)
Goal 1: Predict the extent/dollar-amount of property damage from any given attack.

Goal 2: Identify factors that could predict the target/victim type in an attack.

### Sean Kugele (Data Scientist, Team Lead)
Goal 1: Predict the group responsible for perpetrating a terrorist attack.

Goal 2: Estimate the probability of an attack based on temporal and geo-spatial variables.

Goal 3: Identify *clusters* of incidents that reveal interesting patterns in the data.

## III. Methods of Evaluation
We could use stepwise regression to find the best predictors in any given model. Under such a method, we could use criteria like AIC and BIC to evaluate the fit. A potential function that could be used is the recursive feature elimination function in the R-package "caret".

## IV. Potential (General) Difficulties and Concerns
Preliminary analysis of the dataset has revealed that the data is sparsely populated and will require a significant amount of data cleansing before it will be usable. Many of the variables are also redundant; therefore, the actual number of usable variables is likely closer to 90.

The data set spans over four decades of observations, and the data was collected by multiple agencies. As a result, the data may be inconsistent in quality, and the data collection methodology that was used may have changed over time. These factors will likely introduce sources of irreducible errors into models generated from this data. 

According to the documentation, many of the variables were introduced after 1997; therefore, we will need to make a case-by-case decision about the inclusion of these older observations, depending on the nature of the research question and the needed features.

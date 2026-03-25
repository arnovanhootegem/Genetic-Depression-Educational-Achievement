# Genetic-Depression-Educational-Achievement

R and Mplus scripts for paper on the "Genetic Risks for Depression and Educational Achievement in Adolescence" by Arno Van Hootegem, Tina Baier, Jan Paul Heisig & Torkild Hovde Lyngstad

# My R Scripts
This repository contains a collection of R and Mplus scripts that I have made for the paper "Genetic Risks for Depression and Educational Achievement in Adolescence", written by Arno Van Hootegem, Tina Baier, Jan Paul Heisig and Torkild Hovde Lyngstad. It uses a genetically informed design to examine associations between genetic liability to major depressive disorder and ninth‑grade educational achievement. Using nearly 38,000 parent–child trios from the Norwegian Mother and Child Cohort Study linked to administrative test scores, we estimate direct effects of adolescents’ depression polygenic scores (PGS) and indirect effects of parental depression PGSs (sometimes described as “genetic nurture”), controlling for adolescents and parental educational‑attainment PGSs. 

# Scripts
For the main analyses:
- `Data preparation - Cleaned - Revised.R`: Prepares data by linking genetic data to administrative registers. It also includes descriptive statistics. 
- `CFA depression - Revised.inp`: Mplus script conducting a Confirmatory Factor Analysis on parental depresstive symptoms
- `Model 1 - Revised.inp`: Mplus script estimating Model 1
- `Model 2 - Revised.inp`: Mplus script estimating Model 2
- `Model 3 - Revised.inp`: Mplus script estimating Model 3
- `Model 4 - Revised.inp`: Mplus script estimating Model 4
- `Model 5 - Revised.inp`: Mplus script estimating Model 5
- `SEM model - Revised.inp`: Mplus script estimating a Structural Equations Model with parental education, income and depressive symptoms as mediators

For the sensitivity analyses: 
- `Model 1 - Revised - GPA.inp`: Mplus script estimating Model 1 on Grade Point Average
- `Model 2 - Revised - GPA.inp`: Mplus script estimating Model 2 on Grade Point Average
- `Model 3 - Revised - GPA.inp`: Mplus script estimating Model 3 on Grade Point Average
- `Model 4 - Revised - GPA.inp`: Mplus script estimating Model 4 on Grade Point Average
- `Model 5 - Revised - GPA.inp`: Mplus script estimating Model 5 on Grade Point Average
- `SEM model - Revised - GPA.inp`: Mplus script estimating a Structural Equations Model on Grade Point Average with parental education, income and depressive symptoms as mediators
- `Model 5 - Revised - By test.inp`: Mplus script estimating Model 5 by mathematics and reading scores separate
- `Model 5 - Revised - By gender.inp`: Mplus script estimating Model 5 by gender


# Data
Norwegian privacy regulations limit our ability to share our register data. Individual researchers may apply to obtain permissions and subsequently access the data. We can provide guidance on how to request access to these data.

# Feedback
Feel free to reach out if you have suggestions to improve the code or if there are things that are not understandable

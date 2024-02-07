# Merged Structure of a Simulation Study

### Goal

The overall goal of this project is to develop and test an (adversarial) collaborative approach for conducting simulation studies (in the context of psychological methods). This aims to highlight subjectivity-prone decision-making elements and increase their generalizability. Thus, we propose a basic and general structure of simulation studies that serves as a foundational framework within which collaborative processes can take place. Hence, this structure should be (1) sufficiently modular, to allow for independent evaluation and modification of critical decision-making processes by collaborators, (2) maximally precise and clear to enable structured evaluation of the collaborative process and (3) comprehensive and complete to ensure scientifically rigorous execution and reporting. In general we aim to ensure compatible with other proposed structures.

### Structure

1. Defining aims and objectives / Research Question of Interest. ( = verbal description)
    - as specific as possible
    - e.g. examination of goodness-of-fit statistics under varying degrees of misspecification
    - comparison of the maximum likelihood (ML) to (2SLS)
2. Specification of Population Model 
    - **Optional** and depending on field of simulation
        
        Modularities:
        
    - Structure: (e.g. CFA or SEM)
    - Size: number of latents and indicators
    - Complexity: (cross-loaded indicators, Reciprocal paths, Exogenous predictors)
    
    → select target model for (assumed) *general* model types

            
3. Data Generation Mechanism
    - resampling vs. parametric model draw
    - random number draw for data generation
    
4. Experimental Design Simulation Procedures
    - Determine what factors to vary, on what levels, and whether fully, or partly factorially or one at a time (factor = scenario in Morris et al., 2019)
    - e.g.
        - sample size
        - distribution of the observed variables
        - extent of misspecification
            
            …

5. Method Selection
    - Varies depending on research question
    - e.g.
        - type of and number of estimation methods to be compared
6. Defining Estimands / Population level values
    - should reflect values commonly encountered in applied research.
    - e.g.
        - R2 values the chosen coefficients produce should also be reasonable for applied research.
        - parameters of the model should be statistically significant, even at the smallest sample size of the simulation.
        - consider power issues (e.g.:enough power to detect misspecification, too much power to detect misspecification at all sample sizes?)
        - “bias” in the estimates that will be introduced by the misspecification.
7. Performance Measures 
    - Selection and justification of use of e.g.
        - Bias
        - sensitivity/ specificity
        - predictive accuracy
    - decision on number of simulations for acceptable Monte Carlo SE for these measures
8. Software Selection
    - to run simulation
    - packages & functions
9. Analysis and Interpretation plan
    - Analysis: descriptive vs. inferential
    - Interpretation: decision criteria that evaluate performance : e.g. if 1-ß > 90, the method performs well

Before Coding and Execution: Anticipating all critical decision processes (e.g. exclusion criteria for “imperfect” samples)

10. Coding and Execution 
    - Amount and content of scripts
    - include (sanity) checks
    - setting seeds
    - troubleshooting & verification
11. Analyzing results
    - descriptive
    - graphical
    - inferential
    - exploration
12. Reporting & Presentation
    - provide rationales for each choice made in the previous steps
    - publishing code and simulated data

### Background

Firstly, it is important to consider that the single steps of the above structure are not meant to be equally comprehensive or relevant but are rather a sequence of decisions to be taken chronologically.

Our structure is mainly oriented on the structure proposed by Paxton et al. (2001). Along with the fact that the authors are focussing on the context of structural equation modelling in social sciences, we found their structure to be very detailed. Both these elements align with our goals. Nevertheless, they have a clear focus on the execution and coding of the simulation, partly neglecting preparation and planning. For this reason, as well as a lack of explicit distinction between these two phases, we added elements proposed by Morris et al. (2019). Additionally, we included the last step, Reporting & Presentation, which was emphasized by many other researchers to be important in the context of open science and reproducibility. (Morris et al., 2019, Boomsma, 2013; Monks et al., 2019; Smith & Marshall, 2010).

As mentioned before, part of our goal was to ensure compatibility with other proposed structures  In this sense, although differing in the details on an abstract level, our proposed structure is compatible with structures proposed across a range of research contexts of simulations studies (e.g. Burton et al., 2006; Harwell et al., 1996, Shannon, 1992).

### Conclusion

In sum the proposed structure constitutes a comprehensive, precise and modular set of decision making elements as a foundational framework for collaboratively conducting a simulation study. However it is detached from a specific research topic and might be adapted to its specific intricacies. The same is true for when we decide on a refined collaboration procedure.

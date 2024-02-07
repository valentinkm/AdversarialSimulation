# Procedure for Adversarial Collaboration of Simulation studies

# Goal

The following is a proposal for the general procedure and design for conducting adversarial collaboration of a simulation study that aims to evaluate two competing methods. 

The goal is a framework that allows structured and streamlined collaboration between two or more researchers. It is aimed at increasing generalizability, reducing ambiguity and resolving or clarifying the core aspects of any disagreements. Here we outline the general intended procedure, identify suitable adversarial collaboration techniques and elaborate on documentation as well as evaluation of the collaborative process. Finally, based on the steps proposed in our [structure of simulation studies](https://github.com/valentinkm/RedTeamCFA/blob/main/Merged%20Structure%20of%20a%20Simulation%20Study.md), we provide a more fine-grained and stepwise description of the procedure.

# Outline

In general, the procedure consists of two parts:

**Part 1: Separate Studies**

- Based on one jointly generated research question, each researcher conducts an independent preliminary simulation study within our defined structure.
- This involves *individual simulation protocols*, implementations and interpretations of the study.

**Part 2: Joint Study**

- The main adversarial collaboration occurs here. Each step of the individual simulation studies is scrutinized and debated between collaborators, using adversarial collaboration techniques as listed below.
- Decisions are made and documented based on the most convincing argument presented, if possible.
- This results in a *shared simulation protocol*, implementation and interpretation of the study.
- This process starts based on the results of Part 1, which could include contradictory findings, subjective interpretations of ambiguous results, or strong opinions on expected outcomes.
- In case of fundamentally unresolvable disagreements, a **third neutral arbiter** (Aaron Peikert) should be consulted to try to resolve them.

---

## Adversarial Collaboration (AC) Techniques:

At each round of collaboration, any of the following techniques can be used, depending on their applicability:

- Core Disagreements: Arrive at clearly defined core disagreements in the research question or objectives that might be the origin for conflicts in successive steps. (Clark et al., 2022)
- Assumption check: List, question, and categorize assumptions (Kardos & Dexter, 2017)
- Red-Teaming: what if scenarios (Kardos & Dexter, 2017)
    - e.g. Generate and test alternative estimands with the goal of invalidating the arbiters decisions.
- Quality of information (Kardos & Dexter, 2017): checking the adversaries quality of evidence on a paper-level

## Documentation

The report of the *Joint Study* will be presented in the main paper in a concise way focussing on the most crucial decisions. 

In addition, a separate decision log will serve as a detailed and complete documentation of all decisions made. Here, we summarize the AC-techniques used, as well as their consequence for the decision-making process.

In our mind, decision making can be based on 4 distinct grounds: 

- Evidence
- Pragmatic reasons
- Arbitrary reasons
- Other reasons (e.g. personal values, political issues)

In some cases, more than one of these are present. In such cases, we want to rank their relevance to a decision, if possible.

Example:

“We agree, that the best choice for the second factor sample size is c(50, 100, 200). Primarily, based on Peikert et al (2020) (evidence-based), but also as this was an acceptable average of our individual suggestions (pragmatic reason).”

## Evaluation

The adversarial collaboration in Part 2 (Joint study), is documented and evaluated from each collaborators perspective using semi-structured diary entries based on Shah & Leeder (2016), after each step. This has two purposes:

1. accumulating evidence for the evaluation of the AC and the framework we propose.
2. ideas for improving that framework.
    
    

# Detailed Outline

1. **Defining aims and objectives / Research Question of Interest ( = verbal description)**
- Step 1: Individual Simulation Studies:
    
    exact formulation by the collaborators together to ensure that the research question is clear and unambiguous and that the focus lies on comparing the respective methods on an evidence level
    
    *Contained in individual simulation protocols*
    
- Step 2: Merged Simulation Study:
    
    Result of the adversarial process, incorporating the individual studies results
    
    Using AC techniques
    
    *Contained in shared simulation protocol*
    
2. **Specification of Population Model - 9. Analysis and Interpretation plan**
- Step 1: Individual Simulation Studies:
Chosen independently by each collaborator
    
    *Contained in individual simulation protocols*
    
- Step 2: Merged Simulation Study:
Result of the adversarial process
    
    Using AC techniques
    
    *Contained in shared simulation protocol*
    
10. **Coding and Execution**
- Step 1: Individual Simulation Studies:
Conducted independently by each collaborator
    
    Based on individual simulation protocols
    
- Step 2: Merged Simulation Study:
One script as result of the adversarial process
    
    Based on shared simulation protocol
    
    Using AC techniques
    
11. **Analyzing results**
- Step 1: Individual Simulation Studies:
    
    Based on individual simulation protocols 9th step: Analysis and Interpretation plan
    
- Step 2: Merged Simulation Study:
Based on shared simulation protocols 9th step: Analysis and Interpretation plan
    
    
12. **Reporting & Presentation**

- Step 1: Individual Simulation Studies:
A broad outline of the report and presentation is presented by each collaborator
- Step 2: Merged Simulation Study:
Each side reports on their own

# DisgustDeval
Task assessing goal-directed action using disgust outcome-devaluation. 

======================================

## Now includes:

### Task
- instrumental training .psyexp task
- devaluation test . psyexp task
- M&Ms, BBQ Shapes & Tiny Teddies cockroach movies.
  - NOTE: Devaluation test is designed for fMRI testing. Needs to be adapted for behavioural only assessment. 

  #### Combined Training and Devaluation Test 
  - File: devalCombined.psyexp 
  - Left/Right Responses: 'c'/'m'
  - Includes Instrumental training, Devaluation video and Extinction Test.
  - Includes Counter for Outcomes Earned During Devaluation. 
  - Devaluation adapted for behavioural-only testing. Fixation crosses removed.

  __Counterbalancing__
  --------------------
  
  | Version | Left Outcome | Right Outcome | Devalued Outcome |
  |:-------:|:------------:|:-------------:|:----------------:|
  |A        | M&Ms         | BBQ Mini Crackers | M&Ms         |
  |B        | M&Ms         | BBQ Mini Crackers | BBQ Mini Crackers |
  |C        | BBQ Mini Crackers | M&Ms         | BBQ Mini Crackers |
  |D        | BBQ Mini Crackers | M&Ms         | M&Ms              |



### Analysis
- Instrumental Training Data:
  - Three outcome pleasantness ratings
  - Total number of R1 responses
  - Total number of R2 responses
  - Total number of O1 won
  - Total number of O2 won
  - Total outcomes won

- Post-transfer Devaluation Data:
  - Total number of devalued responses
  - Total number of nondevalued responses

 __TO DO__

 - adapt analysis code for output from devalCombined.psyexp


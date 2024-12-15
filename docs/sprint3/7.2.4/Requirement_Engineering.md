# US7.2.4 As an Admin, I want to add new Medical Condition, so that the Doctors can use it to update the Patient Medical Record.

---

## Table of Contents

- [1. Requirement Engineering](#1-requirement-engineering)

    - [1.1. User Story Description](#12-user-story-description)
    - [1.2. Customer Specifications and Clarifications](#13-customer-specifications-and-clarifications)
    - [1.3 Acceptance Criteria](#14-acceptance-criteria-)
    - [1.4 Other rules](#15-other-rules)
    - [1.5 Dependencies](#16-dependencies)
    - [1.6 Input and Output Data](#17-input-and-output-data)



## 1. Requirement Engineering


### 1.1. User Story Description

As an Admin, I want to add new Medical Condition, so that the Doctors can use it to update the Patient Medical Record.

### 1.2. Customer Specifications and Clarifications

**From the specification document:**


> The specification document initially references the mediacal conditiona as an attribute of the patient

<br>

**From the client clarifications:**

> **Question:** "The medical condition consist in what? Just a name or are there more fields?"
>
> **Answer:** "it consists of a code (for example following ICD (International Classification of Diseases)), a designation and a longer description as well a list of common symptoms"


> **Question:** "Earlier, you said the medical condition needed a code. Is this code automatic or is writen by the admin?"
>
> **Answer:** "it must conform with the classficiation system you select, for instance, SNOMED CT (Systematized Nomenclature of Medicine - Clinical Terms) or ICD-11 (International Classification of Diseases, 11th Revision)"

> **Question:** Boa noite. Qual seria o tamanho máximo de uma designação e descrição de uma alergia? Cumprimentos.
>
>  **Answer:** "designação, max 100 caracteres descrição, máximo 2048 caracteres" 

> **Question:**
Hello,
What are the symptoms for a medical record? Are they a list already present in the system, and when creating the medical record, do you select the symptoms? If yes, what happens when a disease has a symptom that is not in that list? Who creates it in the system?
Thank you.
>
>  **Answer:** "symptoms are free text"
<br>

### 1.3. Acceptance Criteria:



### 1.4. Other rules:



### 1.5. Dependencies:


### 1.6. Input and Output Data:

- **Input data:**

    


- **Output data:**

    

  
---
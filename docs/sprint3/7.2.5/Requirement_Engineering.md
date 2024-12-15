# US7.2.5 As a Doctor, I want to search for Medical Conditions, so that I can use it to update the Patient Medical Record.

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


> The specification document initially references the mediacal condition as an attribute of the patient

<br>

**From the client clarifications:**

> **Question:** "What do you define as Medical Condition? Is it an allergy?"
>
> **Answer:** "they are two different things. a Medical Condition represents a diagnosed health issue or disease. Examples: Diabetes, Hypertension, Asthma, etc."

> **Question:** "Gostaria de lhe perguntar se existe alguma lista de medical conditions que prefere que utilizemos no sistema por default, se sim, quais? Também gostariamos de perguntar se quando diz "I want to search for Medical Conditions, so that I can use it to update the Patient Medical Record" o que é que implica a ultima parte?"
>
> **Answer:** 
>default medical conditions (ICD-11):
> 
>A04.0 Cholera
> 
>A08.0: Rotavirus enteritis
> 
>B20: Human Immunodeficiency Virus (HIV) disease
> 
>B50: Plasmodium falciparum malaria
> 
>2A20.0: Malignant neoplasm of lung
> 
>2F44.0: Malignant neoplasm of the breast
> 
>3A01.1: Iron deficiency anemia
> 
>4A44: Hereditary hemochromatosis
> 
>5A11: Type 1 diabetes mellitus
> 
>5B55: Obesity
> 
>6A80: Major depressive disorder
> 
>6C40: Generalized anxiety disorder
> 
>FB20.1: Osteoporosis with pathological fracture
> 
>FB81.1: Osteoarthritis of the knee
> 
>FB81.2: Osteoarthritis of the hip
> 
>FB80.1: Rheumatoid arthritis
> 
>FA24.0: Fracture of femur
> 
>FA22.0: Fracture of radius and ulna
> 
>FA21.0: Dislocation of shoulder
> 
>FB70.0: Low back pain
>
>quando o médico está a editar o registo médico do paciente, deve ter a possibilidade de inserir entradas de alergias e/ou condições médicas através de pesquisa de alergias/condições médicas

> **Question:** Dear client, Regarding User Story 7.2.5, we would like to confirm how the search for medical conditions should work. Should the search return all registered medical conditions, or should it allow filtering based on a specific parameter? If it’s based on a parameter, could you specify which one?"
>
>  **Answer:** "This requirement is related to the adding/updating of an medical condition entry in the medical record. Thus, when the doctor is adding or editing a medical condition entry, they must be able to search for medical condition by code or designation instead of entering the "id" directly or selecting it from a drop down."



### 1.3 Acceptance Criteria:



### 1.4 Other rules:



### 1.5 Dependencies:


### 1.6 Input and Output Data:

- **Input data:**




- **Output data:**



  
---
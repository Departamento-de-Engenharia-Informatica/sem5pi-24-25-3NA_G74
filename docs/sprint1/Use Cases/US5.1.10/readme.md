

# US 5.1.10 - As an Admin, I want to delete a patient profile, so that I can remove patients who are no longer under care

---

## Table of Contents

- [1. Requirement Engineering](#1-requirement-engineering)
  
    - [1.1. Context](#11-context) 
    - [1.2. User Story Description](#12-user-story-description)
    - [1.3. Customer Specifications and Clarifications](#13-customer-specifications-and-clarifications)
    - [1.4 Acceptance Criteria](#14-acceptance-criteria-)
    - [1.5 Other rules](#15-other-rules)
    - [1.6 Dependencies](#16-dependencies)
    - [1.7 Input and Output Data](#17-input-and-output-data)



## 1. Requirement Engineering

### 1.1. Context

The system will enable hospitals and clinics to manage surgery appointments, and patient records. The patients will be 
managed by the backoffice module. 

The patients represent individuals receiving medical care and is a user of the system.

### 1.2. User Story Description

As an Admin, I want to delete a patient profile, so that I can remove patients who are no longer under care

### 1.3. Customer Specifications and Clarifications

**From the specification document:**

>"The system will enable hospitals and clinics to manage surgery appointments, and **patient records**."
 
>"Overall, the backofffice module will manage:
>
> • Medical professionals (doctors, nurses)
>
>• **Patients**
>
>• Operation types
>
>• Rooms
>
>• Chirurgic requests"

>"User"
>
>"Represents a user in the system, either (i) an admin managing the system, (ii) a healthcare staff
(e.g. doctor, nurse, technician) or (iii) **patients** interacting with it."

>"**Patient** users are self-registered using the IAM."
 
>"The user’s IAM record is linked to the respective user and staff/**patient** record
in the backoffice data."

>"All users authenticate using the IAM."

>**"Patient"**
> 
>"Represents individuals receiving medical care." 
> 
> • Attributes:
> 
>o `First Name`
> 
>o `Last Name`
> 
>o `Full Name`
> 
>o `Date of Birth`
> 
>o `Gender`
> 
>o `Medical Record Number` (unique identifier)
> 
>o `Contact Information` (email, phone)
> 
>o `Allergies/Medical Conditions` (optional)
> 
>o `Emergency Contact`
> 
>o `Appointment History` (list of previous and upcoming appointments)
> 
>• Rules:
> 
>o A **patient** must be unique in terms of `Medical Record Number`, `Email` and
`Phone`.
> 
>o Sensitive data (like medical history) must comply with GDPR, allowing patients
to control their data access.

> "Some anonymized data may be retained for legal or research purposes, but all identifiable information is erased."


<br>

**From the client clarifications:**

> **Question:** "Regarding the use case of the admin being able to delete the ... Is it 5 or 10? The admin being able to delete a patient's profile."
>
> **Answer**: "Therefore, during the patient's journey, there may have already been persistence of data regarding operations and requests for operations and other related data. 
>
> When we delete the user's profile, the patient's profile in this case, what is supposed to happen to everything related to it? This is where the GDPR part comes in, obviously. And the entire legislation part, which is what can be deleted and when it can be deleted.
>
>"These operations cannot be carried out at any time, right? It is not because the person has just been treated and left the hospital that they say, I want my data to be deleted. It is not possible. The data must be stored and there are legal periods for this and only after this legal period can they be, in fact, destroyed or anonymized."
> 
>"And that's exactly what we want you to think about in light of what you're learning in the TPs of LAPR 5 and to define, okay, so there's a minimum data retention period. After this data retention period, in fact, we can delete the remaining data. And when you delete it, you obviously delete all related data."


> **Question** : "Either anonymize, delete or anonymize. Sorry, just to clarify and try to understand. Because I consider that there are two contexts here, right? Maybe it will be the same."
>
> **Answer** : There is one that is, we have data, ready, that we would like to no longer be in the system, right? For no one to consult. But there is also a production of data from the course of the operation, right? That does not necessarily belong to the user. In other words, I had surgery on September 28th and this user, this patient, this staff were there.
>
>When I delete my patient later, I also want to erase this record that there was an operation that day, in that room. Okay, that's it. That's why you have to think carefully about how you're going to deal with this difficult case.
>
>Because there are actually different implications here, right? The information that the surgery took place is important. And, again, there are legal deadlines for that. Now, I also hardly want to know that that surgery took place 20 years ago.
>
>And it was ABCOD, there it was. That's what we also need to understand. It's what the validity of the information is.
>
>And that's why I was saying. The information that will have to be deleted, the other that will have to be anonymized. Okay.
>Okay, I get it. I get it. Okay.

> **Question:** The document states "Some anonymized data may be retained for legal or research purposes, but all identifiable information is erased". Which information should be retained?
>
> **Answer:** it is part of the team's responsibility in the scope of the GDPR module to define the policy



> **Question:** "Can the same user have both a patient and a healthcare profile?" 
> 
> **Answer:** "No, no, no. No? Okay. Yeah, no."
> "Let's make it simple. Of course, in a real situation, that would be the case, okay? But let's make it simple, and an employee of a healthcare collaborator has its own identification. If he goes to the medical as a patient, he will have another identification, okay? Okay"

>**Question:** "Q: Can users hold multiple roles?"
> 
>**Answer**: "No, each user can have only one role." 



<br>

### 1.4 Acceptance Criteria: 

- Admins can search for a patient profile and mark it for deletion.
- Before deletion, the system prompts the admin to confirm the action.
- Once deleted, all patient data is permanently removed from the system within a predefined
  time frame.
- The system logs the deletion for audit and GDPR compliance purposes.

### 1.5 Other rules:

- The deletion of patient profiles must comply with legal regulations, such as GDPR, which mandate data retention periods.

- Data anonymization: After the legal retention period, the data may be anonymized before deletion.

- Related data: Deleting a patient profile also involves deleting related data, such as operation records and request for operations.

### 1.6 Dependencies:

- There is a dependency with 5.1.1 because the patient record (principaly the e-mail) must be known by the system.

- There is a dependency with US 5.1.3 because the patients must first be registered and have a patient online profile.

### 1.7 Input and Output Data:

- **Input data:**
 
  - `Patient Profile` to be deleted

- **Output data:**
    
  - Confirmation of patient profile deletion success or failure

---


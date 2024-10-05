

# US 5.1.3 - As a Patient, I want to register for the healthcare application, so that I can create a user profile and book appointments online. 

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

As a Patient, I want to register for the healthcare application, so that I can create a user profile and book appointments online.

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

> "Assume all other medical and **patient** management is managed in other parts of the system not
part of this prototype. This prototype’s scope is concerned with the appointment of surgeries."

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

<br>

**From the client clarifications:**

> **Question:** "Can the same user have both a patient and a healthcare profile?" 
> 
> **Answer:** "No, no, no. No? Okay. Yeah, no."
> "Let's make it simple. Of course, in a real situation, that would be the case, okay? But let's make it simple, and an employee of a healthcare collaborator has its own identification. If he goes to the medical as a patient, he will have another identification, okay? Okay"

> **Question:** "When it comes to patients and healthcare staff, they both have one of the rules that says that some of their, like, attributes need to be unique, and they both need their email to be unique. When they say that the email needs to be unique, we're talking about the email that's also associated to their user has to be unique across all users?" 
>
> **Answer** "Yes. So the email is the identifying attribute, right? For users, or is it the username? It's the username, okay? But typically, as you know, nowadays, most of the usernames that you have in all the systems are your email, okay? So they hack, kind of, instead of you allowing to create a specific username, you use your own email as the username."
>"Okay, so even though it's not the identity, sorry. Yeah, so the idea is that you have the username, but you should use the email as the username, okay? Got it. Yeah, good." 

>**Question:** "There is 5.1.3 and 5.1.8. And the three one is to create when the patient registers, they create a profile. And the other one is that an admin creates a patient profile.
>If a patient creates, registers themselves, but an admin already created a profile for them, does it get assigned based on the email or how does it work? So you're talking about 5.1.3 and 5.1.8. 5.1.3 and 5.1.8, okay. Let me just see how it is. The question here is the profile, I think."
>
> **Answer:**
>Yes. So the overall flow is the following. An administrator must first create the patient record, okay? So you cannot self-register and say that I am a patient.
>So you will already have your patient record created. What you can do afterwards is create your online profile, let's call it that way, so that you can afterwards assess the system to check your appointments, okay? But you need to be first known in the system. So basically this will have a two-factor authentication.
>When you as a patient want to register, want to create a profile register on the system, you will need to check if already there is that same patient with the same record. It will send an email, a separate email, not the one that you are registering, but the email that is already recorded on the system. It will send an email to you with a link so that you can finish the request and you can finish the registration, okay? Was it clear? I think so, but I have another question that arises from the explanation.
>So I can have two different emails? I'm registering with an email and I have another one already, so it's not the same email? No, it's the same email, okay? But the question is, you need to double-check that the email that you are actually inputting in the patient registration is the same that you already have recorded. Okay, that makes more sense. Thanks.
>Yeah, it's not a different one. Yeah, because if it was a different one, I would know how to identify the patient. Yeah, exactly.
>No, no, it's the same. You need to check it. Okay, good.

>**Question:** "Q: Is it mandatory for patients to have a user account to schedule a surgery?"
> 
>**Answer:** "No, patients are not required to have a user account. The system administrator creates patient profiles."

>**Question:** "Q: Can users hold multiple roles?"
> 
>**Answer**: "No, each user can have only one role." 

>**Question:** So we were asking, when we record a patient profile on an operation request or a support or something like that, do we need to create, do we always need to create an associated user or can we leave it just as a patient profile? 
> 
>**Answer:** No, yes, you can leave it just as a patient profile. If by any chance, from a technical point of view, it is easier for the implementation if you also create a user, but the user is inactive or something like that, that is totally ok, because that would be some kind of implementation detail. But from a functional point of view, from a requirement point of view, one thing is to create the patient records, and another thing is the patient user.
>So, these are two different things, okay? And the patient record is always needed, the patient username, only if the patient actually wants to have his own username, so that we can verify the application. Okay, okay, okay, thank you, that's all I need. Okay, thank you. 
 
>**Question:** "What are the system's password requirements?"
> 
> **Answer:** "at least 10 characters long, at least a digit, a capital letter and a special character"

>**Question:** "1. o user apenas pode ser um staff ou patient ou poderá ser algo mais? 2. o user tem a contact information, email e phone, ambos são obrigatórios?" 
> 
> **Answer:** 1. o user apenas pode ser um staff ou patient ou poderá ser algo mais? os utilizadores do sistema são os administradores, as enfermeiras e os médicos, bem como os pacientes (com limitações de funcionalidade) 2. o user tem a contact information, email e phone, ambos são obrigatórios?  sim

>**Question:** "Can you give an example of a Patient's medical record?" 
> 
>**Answer:** See: https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=31320


<br>

### 1.4 Acceptance Criteria: 

- Patients can self-register using the external IAM system.

- During registration, patients provide personal details (e.g., name, email, phone) and create a
  profile.

- The system validates the email address by sending a verification email with a confirmation link.
 
- Patients cannot list their appointments without completing the registration process.

### 1.5 Other rules:

- A patient must be unique in terms of `Medical Record Number`, `Email` and `Phone`.
- Sensitive data (like medical history) must comply with GDPR, allowing patients to control their data access.

### 1.6 Dependencies:

 - There is a dependency with US 5.1.1 because an admnistrator must first create a patient record that is later associated
with a patient online profile, in other works, the patient must be known by the system, a two-factor authentication based on the email.

### 1.7 Input and Output Data:

- **Input data:**
 
  - `Email`  (for system two-factor authentication)
  - `First Name`
  - `Last Name`
  - `Full Name`
  - `Date of Birth`
  - `Gender`
  - `Contact Information` (email, phone)
  - `Allergies/Medical Conditions` (optional)
  - `Emergency Contact`


- **Output data:**
    
  - Patient profile

---


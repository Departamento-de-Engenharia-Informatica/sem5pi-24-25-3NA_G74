

# US 5.1.11 - As an Admin, I want to list/search patient profiles by different attributes, so that I can view the details, edit, and remove patient profiles. 

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

As an Admin, I want to list/search patient profiles by different attributes, so that I can view the details, edit, and remove patient profiles.

### 1.3. Customer Specifications and Clarifications

**From the specification document:**

>"The system will enable hospitals and clinics to manage surgery appointments, and **patient records**."

>"**Patient** users are self-registered using the IAM."
 
>"The user’s IAM record is linked to the respective user and staff/**patient** record
in the backoffice data."


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

> **Question:** Professor, the project says that the administrator can list any type of profile and have access to it. Then, it says that he can search for these profiles by name, email, specialization, but there is also mention of filters to refine the results.
>
> **Answer:** Can you talk a little bit more about that? Can you tell me what the use case is, please? I want to edit, I want to create, I want to list, probably 5.1.15, right? Yeah. Yeah, okay. So, I think that's it.
>
>My question was about the filters, these filters called to apply. The filters are the search criteria. So think about it, I want to list everything, so there are no filters.
>
>So I want to have a filter where I only list doctors, or only list doctors, or only list doctors who belong to a specialization, whether it's a doctor or a physician. Or I want to have a combination of that. I want to look for whether this is a doctor or a physician, or what specialization this person is in.
>
>Okay, okay. So, this is the kind of filter we want. Right.




> **Filters for Profile Search**:
> 
> **Question**: What types of filters can be applied when searching for profiles?
>
> **Answer**: Filters can include doctor specialization, name, or email to refine search results.

<br>

### 1.4 Acceptance Criteria: 

Acceptance Criteria:

- Admins can search patient profiles by various attributes, including name, email, date of birth,
  or medical record number.

- The system displays search results in a list view with key patient information (name, email, date
of birth).

- Admins can select a profile from the list to view, edit, or delete the patient record.

- The search results are paginated, and filters are available to refine the search results

### 1.5 Other rules:

- N/A

### 1.6 Dependencies:

- There is a dependency with 5.1.1 because the patient record (principaly the e-mail) must be known by the system.

- There is a dependency with US 5.1.3 because the patients must first be registered and have a patient online profile.


### 1.7 Input and Output Data:

- **Input data:**
 
  - Attributes that can be used as search filter:
    - `Email`  (for system two-factor authentication)
    - `Medical record number`
    - `First Name`
    - `Last Name`
    - `Full Name`
    - `Date of Birth`
    - `Gender`
    - `Contact Information` (email, phone)
    - `Emergency Contact`


- **Output data:**
    
  - List of patient profiles according to the search filter

---


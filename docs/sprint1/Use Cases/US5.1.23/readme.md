

# US 5.1.23 - As an Admin, I want to list/search operation types, so that I can see the details, edit, and remove operation types.

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

Operation Type - Represents predefined types of medical operations or procedures.

### 1.2. User Story Description

As an Admin, I want to list/search operation types, so that I can see the details, edit, and remove operation types.

### 1.3. Customer Specifications and Clarifications

**From the specification document:**

>"The system will enable hospitals and clinics to manage **surgery** appointments, and patient records."
 
>"Overall, the backofffice module will manage:
>
> • Medical professionals (doctors, nurses)
>
>• Patients
>
>• **Operation types**
>
>• Rooms
>
>• Chirurgic requests"

>"**Operation Type**"
> 
> Represents predefined types of medical operations or procedures.
>
>• Attributes:
>- `ID` (unique identifier)
>- `Name` (e.g., appendectomy, heart bypass)
>- `Required Staff by Specialization` (list of essential staff in respect to
  specialization)
>- `Estimated Duration` (of the operation type)

<br>

**From the client clarifications:**

>**User Story 23**:
>- **Question**: Should actions like removing an operation type be accessed only through specific methods?
>- **Answer**: Yes, operations like removal or deactivation should be available via specific API methods.

>**Deactivation vs Removal**:
>- **Question**: Is removing an operation type the same as deactivating it?
>- **Answer**: Yes, deactivating makes the operation type unavailable for future use but retains historical data.

> **Question:** The next one is about removing operation types.
This is how the user story is described, but in the acceptance criteria, the concept of deactivation is introduced. So, is removing actually deactivating the type of operation? 
>
> **Answer**: Yes, the question is, you need to think in terms of timeline. So, I might have a specific type of operation, say, some kind of leg surgery.
>
>But for some reason, I decided that my hospital would no longer do this type of leg surgery. But I have done it in the past. So, I can't really remove the type of leg surgery.
>
>What I can do is disable this type of operation, so that it is no longer available, so that doctors can no longer order this type of operation. But if I look at the data from the past, and if I have any leg surgery operations, of course I will have the type of leg surgery operation as well. But it is disable.
>
>So, remove, in this sense, is to deactivate.

> **Question:** Could you provide more details on how the attribute Required Staff by Specialisation in Operation Type works? Specifically, how is this list defined for each operation type? For example:
>
>- Are there specific rules or criteria that determine the essential staff for a procedure based on specialisation?
>
>- Is the list fixed for each operation type, or is it dynamically determined based on the availability of staff or other factors?
>
>- Should this attribute save Staff elements or should it focus on specialisations needed for the appointment type?
>
> **Answer:** please see the document https://moodle.isep.ipp.pt/mod/resource/view.php?id=233565

> **Question**: Regarding the required Staff, what is it? A list that defines the specializations and roles of the staff involved in the appointment? Like 2 heart doctors and 5 heart nurses?
> 
> **Answer:** yes

> **Question:** What filters are you looking for in the search/list of staff and patient profiles? And what about operation types?
> 
> **Answer:** answered in 2024.10.04 class (https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=31495)

<br>

### 1.4 Acceptance Criteria: 

- Admins can search and filter operation types by name, specialization, or status
  (active/inactive).

- The system displays operation types in a searchable list with attributes such as name, required
  staff, and estimated duration.

- Admins can select an operation type to view, edit, or deactivate it

### 1.5 Other rules:

- N/A

### 1.6 Dependencies:

 - There is a dependency with US 5.1.20 because an admnistrator must first create/add new types of operations that can reflect the
   available medical procedures in the system in order for them to be listed.

### 1.7 Input and Output Data:

- **Input data:**

  - Attributes that can be used as search filter:
    - `ID` (unique identifier)
    - `Name` (e.g., appendectomy, heart bypass)
    - `Required Staff by Specialization` (list of essential staff in respect to specialization)
    - `Estimated Duration` (of the operation type)
  


- **Output data:**
    
  - List of Operation Types according to the search filter

  
---


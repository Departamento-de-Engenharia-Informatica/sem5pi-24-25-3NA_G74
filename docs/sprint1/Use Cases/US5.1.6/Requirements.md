# US 5.1.4 - As a Patient, I want to update my user profile, so that I can change my personal details and preferences

## Table of Contents

- [US 5.1.4 - As a Patient, I want to update my user profile, so that I can change my personal details and preferences](#us-514---as-a-patient-i-want-to-update-my-user-profile-so-that-i-can-change-my-personal-details-and-preferences)
  - [Table of Contents](#table-of-contents)
  - [Requirement Engineering](#requirement-engineering)
    - [Context](#context)
    - [User Story Description](#user-story-description)
    - [Customer Specifications and Clarifications](#customer-specifications-and-clarifications)
      - [From the specification document](#from-the-specification-document)
      - [From the client clarifications](#from-the-client-clarifications)
    - [Acceptance Criteria](#acceptance-criteria)
    - [Other rules](#other-rules)
    - [Dependencies](#dependencies)
    - [Input and Output Data](#input-and-output-data)

## Requirement Engineering

### Context

The system will enable hospitals and clinics to manage surgery appointments, and patient records. The patients will be
managed by the backoffice module.

The patients represent individuals receiving medical care and is a user of the system.

### User Story Description

As a Patient, I want to update my user profile, so that I can change my personal details and preferences.

### Customer Specifications and Clarifications

#### From the specification document

>The system will enable hospitals and clinics to manage surgery appointments, and **patient records**.

>Overall, the backofffice module will manage:
>
> • Medical professionals (doctors, nurses)
>
>• **Patients**
>
>• Operation types
>
>• Rooms
>
>• Surgery requests

> Assume all other medical and **patient** management is managed in other parts of the system not part of this prototype. This prototype’s scope is concerned with the appointment of surgeries.

>User
>
>Represents a user in the system, either (i) an admin managing the system, (ii) a healthcare staff (e.g. doctor, nurse, technician) or (iii) **patients** interacting with it.

>**Patient** users are self-registered using the IAM.

>The user’s IAM record is linked to the respective user and staff/**patient** record in the backoffice data.

>All users authenticate using the IAM.

>**Patient**
>
>Represents individuals receiving medical care.
>
>- Attributes:
>
>   - `First Name`
>
>   - `Last Name`
>
>   - `Full Name`
>
>   - `Date of Birth`
>
>   - `Gender`
>
>   - `Medical Record Number` (unique identifier)
>
>   - `Contact Information` (email, phone)
>
>   - `Allergies/Medical Conditions` (optional)
>
>   - `Emergency Contact`
>
>   - `Appointment History` (list of previous and upcoming appointments)
>
>- Rules:
>
>   - A **patient** must be unique in terms of `Medical Record Number`, `Email` and
`Phone`.
>
>   - Sensitive data (like medical history) must comply with GDPR, allowing patients
to control their data access.

#### From the client clarifications

> **Question:** Do we always need to create an associated user when recording a patient profile in a medical facility?
>
> **Answer:** No. A patient profile can be created without an associated user unless it's easier technically to create an inactive user.

> **Question:** Can patients update both their user and patient profile information?
>
> **Answer:** Patients can update contact information but not medical details. Changes must be verified and validated.

> **Question:** Is it mandatory for patients to have a user account to schedule a surgery?
>
> **Answer:** No, patients are not required to have a user account. The system administrator creates patient profiles.

> **Question:** Can a user have both patient and healthcare staff profiles?
>
> **Answer:** No, a user cannot have both profiles. Staff and patients have separate identifications.

> **Question:** How are duplicate patient profiles handled when registered by both the patient and admin?
>
> **Answer:** The system checks the email for uniqueness. The admin must first create the patient record, and then the patient can register using the same email.

### Acceptance Criteria

- Patients can log in and update their profile details (e.g., name, contact information, preferences).
- Changes to sensitive data, such as email, trigger an additional verification step (e.g., confirmation email).
- All profile updates are securely stored in the system.
- The system logs all changes made to the patient's profile for audit purposes.

### Other rules

- A patient must be unique in terms of `Medical Record Number`, `Email` and `Phone`.
- Sensitive data (like medical history) must comply with GDPR, allowing patients
to control their data access.

### Dependencies

- There is a dependency with US's 5.1.8, 5.1.3 and 5.1.7 because the Admin needs to create the Patient profile, then the Patient needs to register to that profile, then the Patient needs to be able to login, and only then can the Patient update their profile.

### Input and Output Data

- **Input data:**

  - `Name`
  - `Contact Information` (email, phone)
  - `Preferences` (GDPR, marketing)

- **Output data:**

  - Confirmation of patient profile update success or failure
  - Patient profile

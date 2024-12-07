
US 7.2.2

What information is to be known in an Allergy? Like designation, and anything more?

it consist of a code (for instance, SNOMED CT (Systematized Nomenclature of Medicine - Clinical Terms) or ICD-11 (International Classification of Diseases, 11th Revision)), a designation and an optional longer description


Acceptance Criteria

        The system must provide an interface for the Admin to add new allergies.
        The system should provide appropriate error messages for invalid or incomplete inputs.
        The system must prevent duplicate allergy names (case-insensitive).
        The Admin must see a success message (e.g., "Allergy successfully added") upon successful submission.
        Only users with the "Admin" role can access the interface to add new allergies.

Test Scenarios

    Test Case 1: Admin successfully adds a new allergy with valid data.
        Steps:
            Log in as Admin.
            Navigate to the "Add Allergy" page.
            Enter "Peanuts" as the Allergy Name, add a description.
            Click "Save."
        Expected Result: Allergy is added, success message is displayed, and "Peanuts" is available in the Doctor's dropdown for patient records.

    Test Case 2: Admin adds a new allergy without a description.
        Steps:
            Log in as Admin.
            Enter "Dust" as the Allergy Name, leave Description empty"
            Click "Save."
        Expected Result: Allergy is added successfully.

2. Negative Tests

    Test Case 3: Required fields validation.
        Steps:
            Log in as Admin.
            Navigate to the "Add Allergy" page.
            Leave all fields empty or partially filled.
            Attempt to click "Save."
        Expected Result: "Save" button is disabled, and error messages are displayed for missing required fields.

    Test Case 4: Duplicate allergy name.
        Steps:
            Add "Pollen" as an allergy.
            Attempt to add another allergy with the name "pollen" (case-insensitive).
        Expected Result: An error message is displayed: "Allergy name already exists."

US 7.2.16

Acceptance Criteria

    The system must provide an interface for Admins to edit existing allergy records.
    The interface must pre-fill the current details of the selected allergy.
    The system should validate the updated inputs for required fields (e.g., allergy name, description).
    The system must prevent duplicate allergy names when updating (case-insensitive).
    Only users with the "Admin" role can access the update interface.

Test Case 1: Admin successfully updates an allergy with valid data.

    Steps:
        Log in as Admin.
        Navigate to the "Allergy List" page.
        Select an existing allergy (e.g., "Peanuts") and click "Edit."
        Update the "Description" field to "An allergy to all kinds of peanuts."
        Click "Save."
    Expected Result:
        The allergy record is updated in the database.
        A success message is displayed: "Allergy successfully updated."
        The updated allergy is visible in the list with the new description.

Test Case 2: Admin updates an allergy without changing any data.

    Steps:
        Log in as Admin.
        Navigate to the "Allergy List" page.
        Select an allergy (e.g., "Dust") and click "Edit."
        Do not modify any fields.
        Click "Save."
    Expected Result:
        No changes are made to the allergy record.
        A success message is displayed: "No changes detected."

Test Case 3: Required fields validation.

    Steps:
        Log in as Admin.
        Navigate to the "Allergy List" page.
        Select an allergy (e.g., "Pollen") and click "Edit."
        Clear the "Name" field.
        Attempt to click "Save."
    Expected Result:
        The "Save" button is disabled or an error message is displayed: "Allergy name is required."

Test Case 4: Duplicate allergy name.

    Steps:
        Log in as Admin.
        Navigate to the "Allergy List" page.
        Select an allergy (e.g., "Peanuts") and click "Edit."
        Change the "Name" field to "Pollen" (an existing allergy name).
        Click "Save."
    Expected Result:
        An error message is displayed: "Allergy name already exists."

Test Case 5: Unauthorized user attempts to access the update interface.

    Steps:
        Log in as a non-Admin user (e.g., Doctor or Patient).
        Attempt to navigate to the "Edit Allergy" page.
    Expected Result:
        The system displays an error message: "Access denied."

Test Case 6: System prevents invalid updates.

    Steps:
        Log in as Admin.
        Navigate to the "Allergy List" page.
        Select an allergy (e.g., "Dust") and click "Edit."
        Enter invalid data in the fields (e.g., leave "Description" empty).
        Click "Save."
    Expected Result:
        The system prevents saving the allergy and displays an error message: "Description is required."

Test Case 7: Changes are immediately reflected in the system.

    Steps:
        Log in as Admin.
        Navigate to the "Allergy List" page.
        Select an allergy (e.g., "Dust") and click "Edit."
        Update the "Name" field to "Dust Allergy."
        Click "Save."
        Navigate back to the "Allergy List" page.
    Expected Result:
        The updated allergy ("Dust Allergy") is visible in the list immediately.


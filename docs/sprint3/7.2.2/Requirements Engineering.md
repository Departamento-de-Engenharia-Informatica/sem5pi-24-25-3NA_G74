
US 7.2.3

Acceptance Criteria

    The system must provide a search interface for Doctors to look up allergies by name.
    The search should support partial matches (e.g., searching "Pea" should return "Peanuts").
    The system must handle case-insensitive searches (e.g., searching "peanuts" should match "Peanuts").
    The system must display appropriate messages if no results are found (e.g., "No allergies found").
    The system must only allow users with the "Doctor" role to access the search interface.
    The results should display allergy details, including name and description.

Test Case 1: Doctor successfully searches for an existing allergy by name.

    Steps:
        Log in as a Doctor.
        Navigate to the "Search Allergies" page.
        Enter "Peanuts" into the search bar.
        Click "Search."
    Expected Result:
    The system displays "Peanuts" along with its description in the results list.

Test Case 2: Search returns results for partial matches.

    Steps:
        Log in as a Doctor.
        Navigate to the "Search Allergies" page.
        Enter "Pea" into the search bar.
        Click "Search."
    Expected Result:
    The system displays "Peanuts" in the results list.

Test Case 3: Search is case-insensitive.

    Steps:
        Log in as a Doctor.
        Navigate to the "Search Allergies" page.
        Enter "peanuts" (lowercase) into the search bar.
        Click "Search."
    Expected Result:
    The system displays "Peanuts" in the results list.

Test Case 4: No results found.

    Steps:
        Log in as a Doctor.
        Navigate to the "Search Allergies" page.
        Enter "UnknownAllergy" (nonexistent allergy) into the search bar.
        Click "Search."
    Expected Result:
    The system displays the message: "No allergies found."

Test Case 5: Unauthorized user attempts to access the search interface.

    Steps:
        Log in as a non-Doctor user (e.g., Admin, Patient).
        Attempt to navigate to the "Search Allergies" page.
    Expected Result:
    The system displays an error message: "Access denied."

Test Case 6: Search results display allergy details.

    Steps:
        Log in as a Doctor.
        Navigate to the "Search Allergies" page.
        Enter "Dust" into the search bar.
        Click "Search."
    Expected Result:
    The system displays "Dust" along with its description.
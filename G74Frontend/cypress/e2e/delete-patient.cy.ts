describe('Delete Patient - End-to-End Tests', () => {
    const mockPatient = {
        name: 'John Doe',
        gender: 'Male',
        dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
        contactInformation: { phoneNumber: '123456789', emailAddress: 'john.doe@example.com' },
        emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' },
    };

    const mockMedicalRecordNumber = '12345'; // Mocked medical record number

    beforeEach(() => {
        // Mock the patient list endpoint
        cy.intercept('GET', '**/patient/find', {
            statusCode: 200,
            body: [mockPatient],
        }).as('getPatients');

        // Mock fetching the medical record number
        cy.intercept('GET', '**/patient/getMedicalRecordNumber*', {
            statusCode: 200,
            body: mockMedicalRecordNumber,
        }).as('getMedicalRecordNumber');

        // Visit the patient list page
        cy.visit('/admin/list-patient');
    });

    it('should open the delete patient popup with the correct email', () => {
        // Reload the page to fetch mock data
        cy.reload();
        cy.wait('@getPatients');

        // Open the delete popup
        cy.get('.delete-button').first().click();

        // Assert popup is visible
        cy.get('.delete-popup').should('exist');

        // Verify the email in the confirmation message
        cy.get('.delete-popup').should('contain.text', mockPatient.contactInformation.emailAddress);
    });

    it('should successfully delete a patient', () => {
        // Mock the delete patient endpoint
        cy.intercept('DELETE', `**/patient/${mockMedicalRecordNumber}`, {
            statusCode: 200,
            body: { message: 'Patient profile deleted successfully.' },
        }).as('deletePatient');

        // Reload the page to fetch mock data
        cy.reload();
        cy.wait('@getPatients');

        // Open the delete popup
        cy.get('.delete-button').first().click();

        // Confirm the deletion
        cy.get('.delete-popup button').contains('Delete').click();

        // Wait for the delete request
        cy.wait('@deletePatient');

        // Verify success message
        cy.get('.delete-popup').should('contain.text', 'Patient profile deleted successfully.');

        // Ensure the popup closes
        cy.get('.delete-popup').should('not.exist');
    });

    it('should show an error message if the delete fails', () => {
        // Mock the delete patient endpoint to return an error
        cy.intercept('DELETE', `**/patient/${mockMedicalRecordNumber}`, {
            statusCode: 500,
            body: { message: 'Failed to delete patient.' },
        }).as('deletePatientFailure');

        // Reload the page to fetch mock data
        cy.reload();
        cy.wait('@getPatients');

        // Open the delete popup
        cy.get('.delete-button').first().click();

        // Confirm the deletion
        cy.get('.delete-popup button').contains('Delete').click();

        // Wait for the delete request
        cy.wait('@deletePatientFailure');

        // Verify error message
        cy.get('.delete-popup').should('contain.text', 'Failed to delete patient.');

        // Ensure the popup remains open
        cy.get('.delete-popup').should('exist');
    });

    it('should close the delete popup without deleting the patient', () => {
        // Reload the page to fetch mock data
        cy.reload();
        cy.wait('@getPatients');

        // Open the delete popup
        cy.get('.delete-button').first().click();

        // Assert popup is visible
        cy.get('.delete-popup').should('exist');

        // Cancel the deletion
        cy.get('.delete-popup button').contains('Cancel').click();

        // Ensure the popup closes
        cy.get('.delete-popup').should('not.exist');
    });
});

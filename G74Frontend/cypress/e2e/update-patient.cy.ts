describe('Update Patient - End-to-End Tests', () => {
    const mockMedicalRecordNumber = '12345'; // Mocked medical record number
    const mockPatient = {
        name: 'John Doe',
        gender: 'Male',
        dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
        contactInformation: { phoneNumber: '913863649', emailAddress: 'john.doe@example.com' },
        emergencyContact: { name: 'Jane Doe', phoneNumber: '913863649' },
    };

    beforeEach(() => {
        // Mock fetching the medical record number
        cy.intercept('GET', '**/patient/getMedicalRecordNumber*', {
            statusCode: 200,
            body: mockMedicalRecordNumber,
        }).as('getMedicalRecordNumber');

        // Mock the patient list endpoint
        cy.intercept('GET', '**/patient/find', {
            statusCode: 200,
            body: [mockPatient],
        }).as('getPatients');

        // Visit the patient list page
        cy.visit('/admin/list-patient');
        cy.wait('@getPatients'); // Ensure the mock data is loaded
    });

    it('should open the update patient popup with prefilled values', () => {
        // Open the update popup
        cy.get('.update-button').first().click();

        // Assert popup is visible
        cy.get('.update-popup').should('exist');

        // Verify prefilled values in the Update Patient popup
        cy.get('.update-popup #email').should('have.value', mockPatient.contactInformation.emailAddress);
        cy.get('.update-popup #name').should('have.value', mockPatient.name);
        cy.get('.update-popup #dateOfBirth').should('have.value', '1980-05-15'); // Format the date for the input
        cy.get('.update-popup #phoneNumber').should('have.value', mockPatient.contactInformation.phoneNumber);
    });

    it('should successfully update a patient', () => {
        // Mock patient update request
        cy.intercept('PATCH', `**/patient/${mockMedicalRecordNumber}`, {
            statusCode: 200,
            body: {
                name: 'John Doe Updated',
                gender: 'Male',
                dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
                contactInformation: { phoneNumber: '987654321', emailAddress: 'john.doe@example.com' },
                emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' },
            },
        }).as('updatePatient');

        // Open the update popup
        cy.get('.update-button').first().click();

        // Update the fields
        cy.get('.update-popup #name').clear().type('John Doe Updated');
        cy.get('.update-popup #phoneNumber').clear().type('914854321');

        // Submit the update
        cy.get('.update-popup button').contains('Update').click();
        cy.wait('@updatePatient'); // Wait for the mocked update request

        // Verify success message
        cy.get('.update-message').should('contain.text', 'Patient john.doe@example.com updated successfully!');

        // Ensure the popup closes
        cy.get('.update-popup').should('not.exist');
    });

    it('should show an error message if the update fails', () => {
        // Mock patient update request failure
        cy.intercept('PATCH', `**/patient/${mockMedicalRecordNumber}`, {
            statusCode: 500,
            body: { message: 'Server error' },
        }).as('updatePatientFailure');

        // Open the update popup
        cy.get('.update-button').first().click();

        // Update the fields
        cy.get('.update-popup #name').clear().type('John Doe Updated');
        cy.get('.update-popup #phoneNumber').clear().type('914854321');

        // Submit the update
        cy.get('.update-popup button').contains('Update').click();
        cy.wait('@updatePatientFailure'); // Wait for the mocked failure

        // Verify error message
        cy.get('.update-message').should('contain.text', 'Failed to update patient john.doe@example.com.');

        // Ensure the popup remains open
        cy.get('.update-popup').should('exist');
    });

    it('should close the update popup without saving changes', () => {
        // Open the update popup
        cy.get('.update-button').first().click();

        // Assert popup is visible
        cy.get('.update-popup').should('exist');

        // Click cancel button
        cy.get('.update-popup button').contains('Cancel').click();

        // Ensure the popup closes
        cy.get('.update-popup').should('not.exist');
    });
});

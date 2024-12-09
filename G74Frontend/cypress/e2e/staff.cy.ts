describe('Staff Management', () => {
    describe('Create Staff', () => {
        beforeEach(() => {
            // Mock the staff creation endpoint
            cy.intercept('POST', '**/admin/create-staff', {
                statusCode: 201,
                body: {
                    licenceNumber: '12345',
                    name: 'Jane Doe',
                    phoneNumber: '1234567890',
                    contactEmail: 'jane.doe@example.com',
                    staffSpecialization: 'Nurse',
                    status: 'active',
                    availability: '9-5'
                }
            }).as('createStaff');

            // Visit the create staff page
            cy.visit('/admin/create-staff');
        });

        it('should create a new staff member successfully', () => {
            // Fill out the form
            cy.get('#licenceNumber').type('12345');
            cy.get('#name').type('Jane Doe');
            cy.get('#phoneNumber').type('1234567890');
            cy.get('#contactEmail').type('jane.doe@example.com');
            cy.get('#staffSpecialization').type('Nurse');
            cy.get('#status').select('active');
            cy.get('#availability').type('9-5');

            // Submit the form
            cy.get('button[type="submit"]').click();

            // Wait for the API call and check response
            cy.wait('@createStaff')
                .its('response.statusCode')
                .should('eq', 201);

            // Check for success message
            cy.contains('Staff profile created successfully!').should('be.visible');
        });

        it('should prevent submission of invalid form', () => {
            // Submit button should be disabled initially
            cy.get('button[type="submit"]').should('be.disabled');

            // Fill some fields
            cy.get('#licenceNumber').type('12345');
            cy.get('#name').type('Jane Doe');

            // Submit button should still be disabled
            cy.get('button[type="submit"]').should('be.disabled');

            // Enter invalid email
            cy.get('#contactEmail').type('invalid-email');
            cy.get('button[type="submit"]').should('be.disabled');
        });
    });
});
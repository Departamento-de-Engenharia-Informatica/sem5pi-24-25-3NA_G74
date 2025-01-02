describe('Medical Conditions - E2E & Mocked Tests', () => {

    beforeEach(() => {
        cy.visit('/admin/create-medical-condition');
    });

    it('should create a new medical condition with a mock', () => {
        // Intercept the POST request, mock the response to avoid hitting real DB
        cy.intercept('POST', '**/medical-conditions', {
            statusCode: 201,
            body: {
                medicalConditionCode: '123654',
                designation: 'Test Condition',
                description: 'Mocked Description',
                commonSymptoms: 'Mocked Symptoms'
            }
        }).as('mockCreateMC');

        // Fill the form
        cy.get('#medicalConditionCode').type('TEST123');
        cy.get('#designation').type('Test Condition');
        cy.get('#description').type('Mocked Description');
        cy.get('#commonSymptoms').type('Mocked Symptoms');

        // Submit form
        cy.get('#create-mc-form').submit();

        // Wait for mocked call
        cy.wait('@mockCreateMC')
            .its('request.body')
            .should((body) => {
                expect(body).to.have.property('medicalConditionCode', 'TEST123');
                expect(body).to.have.property('designation', 'Test Condition');
                expect(body).to.have.property('description', 'Mocked Description');
                expect(body).to.have.property('commonSymptoms', 'Mocked Symptoms');
            });

        // Check success message on the UI
        cy.get('#form-message').should('contain', 'Medical Condition created successfully!');
    });

});
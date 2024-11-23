describe('Register Patient - End-to-End Tests', () => {
  beforeEach(() => {
      // Visit the patient registration page
      cy.visit('/admin/create-patient');
  });

  it('should display the patient registration form', () => {
      cy.get('#create-patient-container').should('exist');
      cy.get('#create-patient-form').should('exist');
      cy.get('#name').should('exist');
      cy.get('#dateOfBirth').should('exist');
      cy.get('#gender').should('exist');
      cy.get('#phoneNumber').should('exist');
      cy.get('#emailAddress').should('exist');
      cy.get('#emergencyName').should('exist');
      cy.get('#emergencyPhoneNumber').should('exist');
  });

  it('should enable the submit button only when all fields are valid', () => {
      // Initially, the submit button should be disabled
      cy.get('.submit-button').should('be.disabled');

      // Partially fill the form
      cy.get('#name').type('John Doe');
      cy.get('#dateOfBirth').type('1980-05-15');
      cy.get('#gender').select('Male');
      cy.get('#phoneNumber').type('913863649');
      cy.get('#emailAddress').type('john.doe@example.com');

      // The button should still be disabled because not all fields are filled
      cy.get('.submit-button').should('be.disabled');

      // Complete the form
      cy.get('#emergencyName').type('Jane Doe');
      cy.get('#emergencyPhoneNumber').type('913863649');

      // The submit button should now be enabled
      cy.get('.submit-button').should('not.be.disabled');
  });

  it('should successfully submit the patient registration form', () => {
      // Fill in the patient details
      cy.get('#name').type('John Doe');
      cy.get('#dateOfBirth').type('1980-05-15'); // YYYY-MM-DD format
      cy.get('#gender').select('Male');
      cy.get('#phoneNumber').type('913863649');
      cy.get('#emailAddress').type('john.doe@example.com');
      cy.get('#emergencyName').type('Jane Doe');
      cy.get('#emergencyPhoneNumber').type('913863649');

      // Submit the form
      cy.get('.submit-button').click();

      // Assert success message
      cy.get('#form-message').should('contain.text', 'Patient profile created successfully!');

      // Verify the form was reset
      cy.get('#name').should('have.value', '');
      cy.get('#dateOfBirth').should('have.value', '');
      cy.get('#phoneNumber').should('have.value', '');
      cy.get('#emailAddress').should('have.value', '');
      cy.get('#emergencyName').should('have.value', '');
      cy.get('#emergencyPhoneNumber').should('have.value', '');
  });

  after(() => {
      // Navigate to the list page
      cy.visit('/admin/list-patient');

      // Search for the patient by email
      cy.get('#emailAddress').type('john.doe@example.com');
      cy.get('button').contains('Search').click();

      // Wait for the patient list to load
      cy.wait(1000); // Adjust as necessary based on your application's performance

      // Verify the patient is listed
      cy.get('table').should('contain.text', 'john.doe@example.com');

      // Open the delete popup for the patient
      cy.get('.delete-button').first().click();

      // Confirm the deletion
      cy.get('.delete-popup button').contains('Delete').click();

      // Verify success message
      cy.get('.delete-popup').should('contain.text', 'Patient profile deleted successfully.');

      // Ensure the popup closes
      cy.get('.delete-popup').should('not.exist');
  });
});

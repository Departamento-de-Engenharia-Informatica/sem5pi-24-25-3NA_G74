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
  });
  
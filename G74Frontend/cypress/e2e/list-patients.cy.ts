describe('List Patients - End-to-End Tests', () => {
    beforeEach(() => {
      // Visit the patient list page
      cy.visit('/admin/list-patient');
    });
  
    it('should display the patient list page', () => {
      // Check if the patient list container and table are present
      cy.get('h2').contains('Patient List').should('exist');
      cy.get('.filters').should('exist'); // Check for filter form
      cy.get('table').should('exist'); // Check for the table structure
    });
  
    it('should load and display patients in the table', () => {
      // Mock API response for patients
      cy.intercept('GET', '**/patient/find', {
        statusCode: 200,
        body: [
          {
            name: 'John Doe',
            gender: 'Male',
            dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
            contactInformation: { phoneNumber: '123456789', emailAddress: 'john.doe@example.com' },
            emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' },
          },
          {
            name: 'Jane Smith',
            gender: 'Female',
            dateOfBirth: { yearOfBirth: 1990, monthOfBirth: 10, dayOfBirth: 25 },
            contactInformation: { phoneNumber: '555555555', emailAddress: 'jane.smith@example.com' },
            emergencyContact: { name: 'Bob Smith', phoneNumber: '444444444' },
          },
        ],
      });
  
      // Trigger fetchPatients call
      cy.reload();
  
      // Assert table rows for patients
      cy.get('table tbody tr').should('have.length', 2); // Two patients in the mock response
  
      // Verify content in the first row
      cy.get('table tbody tr').eq(0).within(() => {
        cy.get('td').eq(0).should('contain.text', 'John Doe'); // Name
        cy.get('td').eq(1).should('contain.text', 'Male'); // Gender
        cy.get('td').eq(2).should('contain.text', '1980-05-15'); // DOB
        cy.get('td').eq(3).should('contain.text', '123456789'); // Phone
        cy.get('td').eq(4).should('contain.text', 'john.doe@example.com'); // Email
      });
    });
  
    it('should show a message when no patients are found', () => {
      // Mock empty response
      cy.intercept('GET', '**/patient/find', {
        statusCode: 200,
        body: [],
      });
  
      // Trigger fetchPatients call
      cy.reload();
  
      // Check for 'No patients found.' message
      cy.get('.message').should('contain.text', 'No patients found.');
  
      // Ensure the table is not displayed
      cy.get('table').should('not.exist');
    });
  
    it('should allow filtering patients', () => {
      // Mock API response for filtered patients
      cy.intercept('GET', '**/patient/find*', (req) => {
        const nameFilter = req.query['name'] || '';
        const response = nameFilter === 'John' ? [
          {
            name: 'John Doe',
            gender: 'Male',
            dateOfBirth: { yearOfBirth: 1980, monthOfBirth: 5, dayOfBirth: 15 },
            contactInformation: { phoneNumber: '123456789', emailAddress: 'john.doe@example.com' },
            emergencyContact: { name: 'Jane Doe', phoneNumber: '987654321' },
          },
        ] : [];
        req.reply({ statusCode: 200, body: response });
      });
  
      // Enter a filter and search
      cy.get('#name').type('John');
      cy.get('.filters button[type="submit"]').click();
  
      // Assert filtered results
      cy.get('table tbody tr').should('have.length', 1); // One patient matches the filter
  
      // Verify content in the filtered row
      cy.get('table tbody tr').eq(0).within(() => {
        cy.get('td').eq(0).should('contain.text', 'John Doe'); // Name
      });
    });
  });
  
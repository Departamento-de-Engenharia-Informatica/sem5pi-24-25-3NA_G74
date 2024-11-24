describe('Register Operation Request - E2E Test', () =>{
    beforeEach(()=>{
        cy.visit('/doctor/create-operation');
    });

    it('should display the patient registration form', ()=>{
        cy.get('#medicalRecordNumber').should('exist');
        cy.get('#LicenceNumber').should('exist');
        cy.get('#operationTypeId').should('exist');
        cy.get('#operationDate').should('exist');
        cy.get('#Priority').should('exist');
        cy.get('#submit').should('exist');
    });

    it('should enable the submit button only when all fields are valid', () => {
        // Initially, the submit button should be disabled
        cy.get('#submit').should('be.disabled');
  
        // Partially fill the form
        cy.get('#medicalRecordNumber').type('202411000001');
        cy.get('#operationTypeId').type('1');
        cy.get('#operationDate').type('2023-10-25T14:30');
        cy.get('#Priority').select('ElectiveSurgery');
        
  
        // The button should still be disabled because not all fields are filled
        cy.get('#submit', {timeout: 10000}).should('not.be.disabled');
  
        // Complete the form
        //cy.get('#LicenceNumber').type('12348');
  
        // The submit button should now be enabled
        //cy.get('#submit').should('not.be.disabled');
    });
    it('should successfully submit the operations form', () =>{
        cy.get('#medicalRecordNumber').type('202411000001');
        cy.get('#LicenceNumber').type('12348');
        cy.get('#operationTypeId').type('1');
        cy.get('#operationDate').type('2023-10-25T14:30');
        cy.get('#Priority').select('ElectiveSurgery');
        cy.get('#submit').click();

        cy.on('window:alert', (text) => {
            expect(text).to.contains('Operation Request created successfully');
        });
        cy.get('#medicalRecordNumber').should('have.value', '');
        cy.get('#LicenceNumber').should('have.value', '0');
        cy.get('#operationTypeId').should('have.value', '0');
        cy.get('#operationDate').should('have.value', '');
        cy.get('#Priority').should('have.value', null);

    });

    
})
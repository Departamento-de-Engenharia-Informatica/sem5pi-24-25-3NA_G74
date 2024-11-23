describe('List Operation Types', () => {
  beforeEach(() => {
    cy.visit('/admin/list-operationtype');
  });

  it('Should display the list of operation types page', () => {
    cy.get('.filters').should('exist');
    cy.get('table').should('exist');
  });

  it('Should fetch operation types with filters applied', () => {
    cy.get('#name').type('Colectomy');

    cy.get('button[type="submit"]').click();

    cy.get('table tbody tr').should('have.length', 1);

    cy.contains('Colectomy').should('be.visible');
    cy.contains('15').should('be.visible');
    cy.contains('180').should('be.visible');
    cy.contains('grastointestinologist:1; intern:1; anaesthetist:1; instrumenting nurse:1; circulating nurse:2; anaesthetist nurse:1; ma assistant:1').should('be.visible');
  });

  it('Should clear filters and reset the form', () => {
    cy.get('#name').type('TestOperation');
    cy.get('#requiredStaffBySpecialization').type('Surgeon');
    cy.get('#duration').type('30');

    cy.contains('Clear Filters').click();

    cy.get('#name').should('have.value', '');
    cy.get('#requiredStaffBySpecialization').should('have.value', '');
    cy.get('#duration').should('have.value', '');
  });

  it('Should display "No operation types found" if no results are returned', () => {
    cy.get('#name').type('naoexiste');
    cy.get('button[type="submit"]').click();
    cy.contains('No operation types found.').should('be.visible');
  });
});

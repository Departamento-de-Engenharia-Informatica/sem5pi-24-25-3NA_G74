describe('Medical Conditions - E2E & Mocked Tests', () => {
  beforeEach(() => {
    cy.visit('/admin/list-medical-condition'); 
  });

  it('should display a list of medical conditions from the real backend', () => {
    // 1) Wait for possible loading spinner to disappear
    cy.get('.loading', { timeout: 10000 }).should('not.exist');

    // 2) Now conditionally check if we got data or not
    cy.get('body').then(($body) => {
      // If there's a <table> in the DOM, we proceed with table checks
      if ($body.find('table').length > 0) {
        // The table is rendered
        cy.get('table').should('exist');
        cy.get('table thead tr th').then((headers) => {
          expect(headers.eq(0).text()).to.include('Code');
          expect(headers.eq(1).text()).to.include('Designation');
          expect(headers.eq(2).text()).to.include('Description');
          expect(headers.eq(3).text()).to.include('Common Symptoms');
        });

        // Check if we have any rows
        cy.get('tbody tr').then((rows) => {
          if (rows.length > 0) {
            cy.log(`Found ${rows.length} rows in the medical conditions table.`);
          } else {
            // If the table exists but has zero rows, maybe there's some other logic
            cy.contains('No medical conditions found.').should('be.visible');
          }
        });
      } else {
        // 3) If the table does NOT exist, we expect the "No medical conditions found." message
        cy.contains('No medical conditions found.').should('be.visible');
      }
    });
  });
});

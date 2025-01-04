describe('Update Allergy', () => {
  beforeEach(() => {
    window.localStorage.setItem('token', 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIjoiU2VyZ2lvIiwiaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS93cy8yMDA4LzA2L2lkZW50aXR5L2NsYWltcy9yb2xlIjoiQWRtaW4iLCJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9lbWFpbGFkZHJlc3MiOiJzZXJnaW8uY29zc2liYUBnbWFpbC5jb20iLCJleHAiOjE3MzU4MzkyNjAsImlzcyI6Imh0dHBzOi8vbG9jYWxob3N0OjUwMDEiLCJhdWQiOiJodHRwczovL2xvY2FsaG9zdDo1MDAxIn0.w0PvpyPq357Dm34HO4qhKtXo3BPOFNyw8WBgj_n4PMM');
    cy.intercept('GET', '**/allergy*', {
      statusCode: 200,
      body: [
        {
          code: 'ABC001',
          designation: 'Original Condition',
          description: 'Original Desc',
        }
      ],
    }).as('mockListA');

    // 3) Visit the listing page
    cy.visit('/admin/list-allergy');

    // 4) Wait for the GET call (if you used intercept)
    cy.wait('@mockListA');
  });

  it('should update an existing medical condition with a mock PATCH', () => {
    // 1) Intercept the PATCH request to avoid writing to the real DB
    cy.intercept('PATCH', '**/allergy/ABC001', {
      statusCode: 200,
      body: {
        code: 'ABC001',
        designation: 'Updated Mock Allergy',
        description: 'New Mock Description'
      }
    }).as('mockUpdateA');

    // 2) Find the row with code "ABC001" and click the "Update" button
    cy.get('tbody tr')
      .contains('ABC001')
      .parents('tr')
      .within(() => {
        cy.get('.update-button').click();
      });

    // Wait for the popup to be visible
    cy.get('.update-popup', { timeout: 10000 })
      .should('be.visible')
      .within(() => {
        // Now Cypress focuses its commands inside the .update-popup only
        cy.get('#designation')
          .should('be.visible')
          .clear()
          .type('Updated Mock Allergy');

        cy.get('#description')
          .should('be.visible')
          .clear()
          .type('New Mock Description');

        cy.contains('button', 'Update').click();

        cy.contains('Allergy updated successfully!').should('exist');
      });

  });
});

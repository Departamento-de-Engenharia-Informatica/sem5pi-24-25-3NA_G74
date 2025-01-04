describe('Update Medical Condition (Mocked)', () => {
    beforeEach(() => {
        // 1) Make sure the user is recognized as an Admin.
        //    This is a fake token that your decodeToken() method
        //    won’t actually parse, but it’s enough to trigger the role check.
        //    If you want the decode to REALLY parse a valid token, you can
        //    store a real token with "role":"Admin" in its payload.
        window.localStorage.setItem('token', 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIjoiU2VyZ2lvIiwiaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS93cy8yMDA4LzA2L2lkZW50aXR5L2NsYWltcy9yb2xlIjoiQWRtaW4iLCJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9lbWFpbGFkZHJlc3MiOiJzZXJnaW8uY29zc2liYUBnbWFpbC5jb20iLCJleHAiOjE3MzU4MzkyNjAsImlzcyI6Imh0dHBzOi8vbG9jYWxob3N0OjUwMDEiLCJhdWQiOiJodHRwczovL2xvY2FsaG9zdDo1MDAxIn0.w0PvpyPq357Dm34HO4qhKtXo3BPOFNyw8WBgj_n4PMM');

        // 2) If your DB is guaranteed to contain “ABC001”, you can do a real E2E fetch.
        //    Otherwise, intercept the GET request to mock the table data:
        cy.intercept('GET', '**/medical-conditions*', {
            statusCode: 200,
            body: [
                {
                    medicalConditionCode: 'ABC001',
                    designation: 'Original Condition',
                    description: 'Original Desc',
                    commonSymptoms: 'Original Symptoms'
                }
            ],
        }).as('mockListMC');

        // 3) Visit the listing page
        cy.visit('/admin/list-medical-condition');

        // 4) Wait for the GET call (if you used intercept)
        cy.wait('@mockListMC');
    });

    it('should update an existing medical condition with a mock PATCH', () => {
        // 1) Intercept the PATCH request to avoid writing to the real DB
        cy.intercept('PATCH', '**/medical-conditions/ABC001', {
            statusCode: 200,
            body: {
                medicalConditionCode: 'ABC001',
                designation: 'Updated Mock Condition',
                description: 'New Mock Description'
            }
        }).as('mockUpdateMC');

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
                    .type('Updated Mock Condition');

                cy.get('#description')
                    .should('be.visible')
                    .clear()
                    .type('New Mock Description');

                cy.contains('button', 'Update').click();

                cy.contains('Medical Condition updated successfully!').should('exist');
            });
        /*    
        // 4) Click the "Update" button
        cy.contains('button', 'Update').click();

        // 5) Assert the PATCH request body
        cy.wait('@mockUpdateMC').its('request.body').should((body) => {
            expect(body).to.have.property('designation', 'Updated Mock Condition');
            expect(body).to.have.property('description', 'New Mock Description');
        });

        // 6) Confirm success message
        cy.contains('Medical Condition updated successfully!').should('exist');
        */
    });
});

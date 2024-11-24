describe('List All Operation Request - E2E Test', () =>{
    beforeEach(()=>{
        cy.visit('/doctor/list-operation');
    });

    it('should display the operation list page', () => {
        cy.get("#table").should('exist');
        cy.get('#tableColumns').should('exist');
        cy.get('#tableData').should('exist');
    })

    it('should load and display operations in the table', ()=>{
        cy.intercept('GET','**/doctor/list-operation',{
            statusCode:200
        })
    })
})
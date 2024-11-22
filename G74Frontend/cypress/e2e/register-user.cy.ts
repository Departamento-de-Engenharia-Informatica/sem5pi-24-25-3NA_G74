describe('User Register Page Test', () =>{
  beforeEach(() => {
    cy.visit('/admin/register-user')
  });

  it('Should display the registration form', () => {
    cy.get('form').should('be.visible');
    cy.get('#username').should('exist');
    cy.get('#email').should('exist');
    cy.get('#role').should('exist');
    cy.get('button[type="submit"]').should('exist');
  });

  it('Should register a user successfully', () => {
    cy.get('#username').type('testuser');
    cy.get('#email').type('testuser@example.com');
    cy.get('#role').select('Admin');

    cy.get('button[type="submit"]').click();

    cy.contains('User profile created successfully!').should('be.visible');
  });

  it('Should handle backend error gracefully', () => {
    cy.intercept('POST', '**/User/register', {
      statusCode: 400,
      body: { message: 'User already exists' },
    });

    cy.get('#username').type('existinguser');
    cy.get('#email').type('existinguser@example.com');
    cy.get('#role').select('Patient');

    cy.get('button[type="submit"]').click();

    cy.contains('Failed to create new user. User already exists').should('be.visible');
  });

});

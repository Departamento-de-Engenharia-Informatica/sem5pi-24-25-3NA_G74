describe('Update User Profile', () => {
  beforeEach(() => {
    cy.visit('/patient/update-user');
  });

  it('Should show an error message if email is not provided', () => {
    cy.get('button[type="submit"]').click();

    cy.contains('Please enter a valid email.').should('be.visible');
  });

  it('Should fetch user data and show the edit form', () => {
    cy.get('#email').type('existinguser@example.com');
    cy.get('button[type="submit"]').click();

    cy.get('form').should('be.visible');
    cy.get('#username').should('exist');
    cy.get('#newEmail').should('exist');
    cy.get('#role').should('exist');
    cy.get('button[type="submit"]').should('exist');
  });

  it('Should update the user profile successfully', () => {
    cy.intercept('PATCH', '**/existinguser@example.com', {
      statusCode: 200,
      body: { message: 'User profile updated successfully!' },
    }).as('updateUser');

    cy.get('#email').type('existinguser@example.com');
    cy.get('button[type="submit"]').click();

    cy.get('#username').clear().type('updateduser');
    cy.get('#newEmail').clear().type('updateduser@example.com');
    cy.get('#role').select('Doctor');
    cy.get('button[type="submit"]').click();

    cy.contains('User profile updated successfully!').should('be.visible');
  });

  it('Should show an error message if the update fails', () => {
    cy.intercept('PUT', '**/existinguser@example.com', {
      statusCode: 404,
      body: { message: 'Failed to update user profile. Please try again.' },
    }).as('updateError');

    cy.get('#email').type('existinguser@example.com');
    cy.get('button[type="submit"]').click();

    cy.get('#username').clear().type('updateduser');
    cy.get('#newEmail').clear().type('updateduser@example.com');
    cy.get('#role').select('Doctor');
    cy.get('button[type="submit"]').click();

    cy.contains('Failed to update user profile.').should('be.visible');
  });
});

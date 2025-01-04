describe('Allergy - E2E', () => {

  beforeEach(() => {
    cy.visit('/admin/create-allergy');
  });

  it('should create a new allergy with a mock', () => {
    // Intercepta a requisição POST para evitar chamada real ao backend
    cy.intercept('POST', '**/allergy', {
      statusCode: 201,
      body: {
        code: 'A123',
        designation: 'Mocked Allergy',
        description: 'Mocked Description'
      }
    }).as('mockCreateAllergy');

    // Preenche o formulário
    cy.get('#allergyCode').type('A123');
    cy.get('#designation').type('Mocked Allergy');
    cy.get('#description').type('Mocked Description');

    // Submete o formulário
    cy.get('#create-allergy-form').submit();

    // Espera pela requisição mockada
    cy.wait('@mockCreateAllergy')
      .its('request.body')
      .should((body) => {
        expect(body).to.have.property('code', 'A123');
        expect(body).to.have.property('designation', 'Mocked Allergy');
        expect(body).to.have.property('description', 'Mocked Description');
      });

    // Valida a mensagem de sucesso exibida na UI
    cy.get('#form-message').should('contain', 'Allergy created successfully!');
  });

});

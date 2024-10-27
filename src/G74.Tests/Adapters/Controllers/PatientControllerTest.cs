using System.Net.Http;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.DataModel;
using G74.Domain;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Services;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.Configuration;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

[TestSubject(typeof(PatientController))]
public class PatientControllerTest : IClassFixture<WebApplicationFactory<G74.Startup>>
{

    private readonly HttpClient _client;

    public PatientControllerTest(WebApplicationFactory<G74.Startup> factory)
    {
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task UpdatePatient_ExistingPatient_UpdatesSuccessfully()
    {
        var existingPatientDto = new CreatePatientDTO(
            "Afonso", 
            "male", 
            new DateOfBirthDTO(1990, 10, 1),
            new ContactInformationDTO("913283295", "afonso@gmail.com"), 
            new EmergencyContactDTO("931231422")
        );
        Patient patient = new Patient(
            new Name("Afonso"), 
            new MedicalRecordNumber("20241010101"),
            new DateOfBirth(1990, 10, 1), 
            new Gender(Gender.GenderEnum.Male),
            new ContactInformation("913283295", new Email("afonso@gmail.com")), 
            new EmergencyContact("931231422")
        );

        var existingPatient = new PatientDataModel(patient);
        var mockRepo = new Mock<IPatientRepository>();
        mockRepo.Setup(repo => repo.GetPatientByMedicalRecordNumber(It.Is<MedicalRecordNumber>(m => m.MedicalNumber == "20241010101")))
            .ReturnsAsync(existingPatient);
        mockRepo.Setup(repo => repo.UpdatePatient(It.IsAny<PatientDataModel>()))
            .Returns(Task.CompletedTask); 
        
        var mockRepoUser = new Mock<IRepoUser>();
        var patientService = new PatientAppService(mockRepo.Object, mockRepoUser.Object, new MedicalRecordNumberGenerator(mockRepo.Object), new Mock<IConfiguration>().Object);
        var patientController = new PatientController(patientService);
        
        var updatedPatientInfo = new CreatePatientDTO(
            "Updated Name",
            "male",
            new DateOfBirthDTO(1991, 5, 10),
            new ContactInformationDTO("913283296", "updated@gmail.com"),
            new EmergencyContactDTO("931231423")
        );
        
        var response = await patientController.UpdatePatient("20241010101", updatedPatientInfo);
        
        var okResult = Assert.IsType<OkObjectResult>(response.Result);
        Assert.Equal(200, okResult.StatusCode);
    }

    [Fact]
    public async Task UpdatePatient_NonExistingPatient_ReturnsNotFound()
    {
        var mockRepo = new Mock<IPatientRepository>();
        mockRepo.Setup(repo => repo.GetPatientByMedicalRecordNumber(It.IsAny<MedicalRecordNumber>()))
            .ReturnsAsync((PatientDataModel)null); 

        var mockRepoUser = new Mock<IRepoUser>();
        var patientService = new PatientAppService(mockRepo.Object, mockRepoUser.Object, new MedicalRecordNumberGenerator(mockRepo.Object), new Mock<IConfiguration>().Object);
        var patientController = new PatientController(patientService);
        
        var updatedPatientInfo = new CreatePatientDTO(
            "NonExistent",
            "male",
            new DateOfBirthDTO(1990, 10, 1),
            new ContactInformationDTO("913283295", "nonexistent@gmail.com"),
            new EmergencyContactDTO("931231422")
        );
        
        var response = await patientController.UpdatePatient("nonexistent-medical-number", updatedPatientInfo);
        
        var notFoundResult = Assert.IsType<BadRequestObjectResult>(response.Result);
        Assert.Equal(400, notFoundResult.StatusCode);
    }

}
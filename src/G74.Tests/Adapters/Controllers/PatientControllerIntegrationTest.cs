using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.Domain;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Mappers;
using G74.Services;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

public class PatientControllerIntegrationTest
{
    private readonly PatientController _controller;
    private readonly Mock<IPatientRepository> _mockPatientRepository;
    private readonly IPatientAppService _patientAppService;

    public PatientControllerIntegrationTest()
    {
        _mockPatientRepository = new Mock<IPatientRepository>();
        var mockMedicalRecordGenerator = new Mock<IMedicalRecordNumberGenerator>();
        var mockMapper = new PatientMapper();
        var mockConfig = new Mock<IConfiguration>();

        _patientAppService = new PatientAppService(
            _mockPatientRepository.Object,
            mockMapper,
            mockMedicalRecordGenerator.Object,
            mockConfig.Object
        );

        _controller = new PatientController(_patientAppService);
    }

    [Fact]
    public async Task RegisterPatient_ValidPatient_ReturnsCreated()
    {
        // Arrange
        var newPatientDto = new PatientDTO
        {
            Name = "Daniela Soares",
            Gender = "Female",
            DateOfBirth = new DateOfBirthDTO { YearOfBirth = 2004, MonthOfBirth = 7, DayOfBirth = 9 },
            ContactInformation = new ContactInformationDTO("914 134 980", "1221201@isep.ipp.pt"),
            EmergencyContact = new EmergencyContactDTO("Mãe Soares", "926329715")
        };

        _mockPatientRepository
            .Setup(repo => repo.AddPatient(It.IsAny<Patient>()))
            .ReturnsAsync(new Patient(
                new Name("Daniela Soares"),
                new MedicalRecordNumber("202411000001"),
                new DateOfBirth(2004, 7, 9),
                new Gender(Gender.GenderEnum.Female),
                new ContactInformation("914 134 980", new Email("1221201@isep.ipp.pt")),
                new EmergencyContact("926329715", new Name("Mãe Soares"))
            ));

        // Act
        var result = await _controller.RegisterPatient(newPatientDto);

        // Assert
        var actionResult = Assert.IsType<ActionResult<PatientDTO>>(result);
        //var createdResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
        
    }

    [Fact]
    public async Task UpdatePatientLimited_ExistingPatient_ReturnsOk()
    {
        // Arrange
        var medicalRecordNumber = "202407090001";
        var updatedPatientInfo = new PatientDTO
        {
            Name = "Daniela S.",
            Gender = "Female",
            DateOfBirth = new DateOfBirthDTO { YearOfBirth = 2004, MonthOfBirth = 7, DayOfBirth = 9 },
            ContactInformation = new ContactInformationDTO("915 000 000", "daniela.updated@isep.ipp.pt"),
            EmergencyContact = new EmergencyContactDTO("Mãe Soares", "926329715")
        };

        _mockPatientRepository
            .Setup(repo => repo.GetPatientByMedicalRecordNumber(It.Is<MedicalRecordNumber>(m => m.MedicalNumber == medicalRecordNumber)))
            .ReturnsAsync(new Patient(
                new Name("Daniela Soares"),
                new MedicalRecordNumber(medicalRecordNumber),
                new DateOfBirth(2004, 7, 9),
                new Gender(Gender.GenderEnum.Female),
                new ContactInformation("914 134 980", new Email("1221201@isep.ipp.pt")),
                new EmergencyContact("926329715", new Name("Mãe Soares"))
            ));

        _mockPatientRepository
            .Setup(repo => repo.UpdatePatient(It.IsAny<Patient>()))
            .ReturnsAsync(new Patient(
                new Name("Daniela S."),
                new MedicalRecordNumber(medicalRecordNumber),
                new DateOfBirth(2004, 7, 9),
                new Gender(Gender.GenderEnum.Female),
                new ContactInformation("915 000 000", new Email("daniela.updated@isep.ipp.pt")),
                new EmergencyContact("926329715", new Name("Mãe Soares"))
            ));

        // Act
        var result = await _controller.UpdatePatientLimited(medicalRecordNumber, updatedPatientInfo);

        // Assert
        var actionResult = Assert.IsType<ActionResult<PatientDTO>>(result);
        var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
        
    }
}

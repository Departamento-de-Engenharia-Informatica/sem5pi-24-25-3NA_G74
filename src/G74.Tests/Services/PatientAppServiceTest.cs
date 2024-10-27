using System;
using System.Threading.Tasks;
using G74.Adapters.Repositories;
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
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Moq;
using Xunit;

namespace G74.Tests.Services;

[TestSubject(typeof(PatientAppService))]
public class PatientAppServiceTest
{

[Fact]
public async Task UpdatePatient_ExistingPatient_UpdatesSuccessfully()
{
    var options = new DbContextOptionsBuilder<BackofficeAppDbContext>()
        .UseInMemoryDatabase(databaseName: "TestDatabase")
        .Options;

    using var context = new BackofficeAppDbContext(options);
    
    Patient patient = new Patient(
        new Name("Afonso"), 
        new MedicalRecordNumber("20241010101"),
        new DateOfBirth(1990, 10, 1), 
        new Gender(Gender.GenderEnum.Male),
        new ContactInformation("913283295", new Email("afonso@gmail.com")), 
        new EmergencyContact("931231422")
    );

    var existingPatient = new PatientDataModel(patient);
    context.Patients.Add(existingPatient);
    await context.SaveChangesAsync();
    
    var realRepo = new PatientRepository(context);
    
    var mockRepo = new Mock<IPatientRepository>();
    mockRepo.Setup(repo => repo.GetPatientByMedicalRecordNumber(It.Is<MedicalRecordNumber>(m => m.MedicalNumber == "20241010101")))
        .ReturnsAsync(existingPatient);
    
    var patientService = new PatientAppService(
        mockRepo.Object,  
        new Mock<IRepoUser>().Object,
        new MedicalRecordNumberGenerator(mockRepo.Object),
        new Mock<IConfiguration>().Object
    );
    
    var updatedPatientDto = new CreatePatientDTO(
        "Afonso", 
        "male", 
        new DateOfBirthDTO(1990, 10, 1),
        new ContactInformationDTO("913283295", "afonso@gmail.com"), 
        new EmergencyContactDTO("931231422")
    );
    
    var result = await patientService.UpdatePatient("20241010101", updatedPatientDto);
    
    Assert.NotNull(result);
    Assert.Equal("Afonso", result.Name);
    
    var updatedPatient = await context.Patients.FirstOrDefaultAsync(p => p.MedicalRecordNumber.MedicalNumber == "20241010101");
    Assert.NotNull(updatedPatient);
    Assert.Equal("Afonso", updatedPatient.Name.TheName);
}

[Fact]
public async Task UpdatePatient_NonExistingPatient_ThrowsInvalidOperationException()
{
    var options = new DbContextOptionsBuilder<BackofficeAppDbContext>()
        .UseInMemoryDatabase(databaseName: "TestDatabase")
        .Options;

    using var context = new BackofficeAppDbContext(options);
    
    var mockRepo = new Mock<IPatientRepository>();
    mockRepo.Setup(repo => repo.GetPatientByMedicalRecordNumber(It.IsAny<MedicalRecordNumber>()))
        .ReturnsAsync((PatientDataModel)null); 
    
    var patientService = new PatientAppService(
        mockRepo.Object,  
        new Mock<IRepoUser>().Object,
        new MedicalRecordNumberGenerator(mockRepo.Object),
        new Mock<IConfiguration>().Object
    );
    
    var updatedPatientDto = new CreatePatientDTO(
        "NonExistent", 
        "male", 
        new DateOfBirthDTO(1990, 10, 1),
        new ContactInformationDTO("913283295", "nonexistent@gmail.com"), 
        new EmergencyContactDTO("931231422")
    );

    var exception = await Assert.ThrowsAsync<ArgumentException>(
        () => patientService.UpdatePatient("nonexistent-medical-number", updatedPatientDto)
    );
}


}
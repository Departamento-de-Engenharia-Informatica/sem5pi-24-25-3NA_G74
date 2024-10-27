using System;
using System.Linq;
using System.Threading.Tasks;
using G74.Adapters.Repositories;
using G74.DataModel;
using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.DTO;
using Microsoft.EntityFrameworkCore;
using Xunit;
using Email = G74.Domain.Value_Objects.User.Email;

namespace G74.Tests.Adapters.Repositories;

public class PatientRepositoryTest
{
    private async Task<BackofficeAppDbContext> GetInMemoryDbContextAsync()
    {
        var options = new DbContextOptionsBuilder<BackofficeAppDbContext>()
            .UseInMemoryDatabase(Guid.NewGuid().ToString())
            .Options;

        var context = new BackofficeAppDbContext(options);
        await context.Database.EnsureCreatedAsync();
        return context;
    }

    private PatientDataModel CreatePatientDataModel(string name, string medicalRecordNumber,
        bool markForDeletion = false, TimeSpan? deletionTimespan = null)
    {
        var patient = new PatientDataModel(new Patient(
            new Name(name),
            new MedicalRecordNumber(medicalRecordNumber),
            new DateOfBirth(1990, 5, 15),
            new Gender(Gender.GenderEnum.Male),
            new ContactInformation("917654221", new Email("patient@example.com")),
            new EmergencyContact("917654321")
        ));

        if (markForDeletion && deletionTimespan.HasValue)
        {
            patient.MarkForDeletion(deletionTimespan.Value);
        }

        return patient;
    }

    [Fact]
    public async Task AddPatient_AddsPatientSuccessfully()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);
        var patient = CreatePatientDataModel("John Doe", "20241010101");

        // Act
        await repository.AddPatient(patient);
        var result = await context.Patients.FirstOrDefaultAsync(p => p.Name.TheName == "John Doe");

        // Assert
        Assert.NotNull(result);
        Assert.Equal("John Doe", result.Name.TheName);
        Assert.False(result.ToDelete);
    }

    [Fact]
    public async Task GetPatientByMedicalRecordNumber_ReturnsCorrectPatient()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);
        var medicalRecordNumber = "20241010101";
        var patient = CreatePatientDataModel("Jane Doe", medicalRecordNumber);

        context.Patients.Add(patient);
        await context.SaveChangesAsync();

        // Act
        var result = await repository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber));

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Jane Doe", result.Name.TheName);
        Assert.Equal(medicalRecordNumber, result.MedicalRecordNumber.MedicalNumber);
    }

    [Fact]
    public async Task UpdatePatient_UpdatesPatientSuccessfully()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);
        var patient = CreatePatientDataModel("Alice Doe", "20241010102");

        context.Patients.Add(patient);
        await context.SaveChangesAsync();

        // Act
        patient.UpdateName(new Name("Alice Smith"));
        await repository.UpdatePatient(patient);

        var updatedPatient = await context.Patients.FirstOrDefaultAsync(p => p.Name.TheName == "Alice Smith");

        // Assert
        Assert.NotNull(updatedPatient);
        Assert.Equal("Alice Smith", updatedPatient.Name.TheName);
    }

    [Fact]
    public async Task CountAsync_ReturnsCorrectCount()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);

        var patient1 = CreatePatientDataModel("Patient 1", "20241010103");
        var patient2 = CreatePatientDataModel("Patient 2", "20241010104");

        context.Patients.AddRange(patient1, patient2);
        await context.SaveChangesAsync();

        // Act
        var count = await repository.CountAsync();

        // Assert
        Assert.Equal(2, count);
    }

    [Fact]
    public async Task GetPatientsReadyForDeletion_ReturnsCorrectPatients()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);

        var patientToBeDeleted = CreatePatientDataModel("To Be Deleted", "20241010105", markForDeletion: true,
            deletionTimespan: TimeSpan.FromMinutes(-10));
        var patientNotReady = CreatePatientDataModel("Not Ready", "20241010106", markForDeletion: true,
            deletionTimespan: TimeSpan.FromMinutes(10));

        context.Patients.AddRange(patientToBeDeleted, patientNotReady);
        await context.SaveChangesAsync();

        // Act
        var result = await repository.GetPatientsReadyForDeletion();

        // Assert
        Assert.Single(result);
        Assert.Equal("To Be Deleted", result[0].Name.TheName);
    }

    [Fact]
    public async Task DeletePatientDefinitive_RemovesPatientSuccessfully()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);
        var patient = CreatePatientDataModel("Delete Me", "20241010107");

        context.Patients.Add(patient);
        await context.SaveChangesAsync();

        // Act
        await repository.DeletePatientDefinitive(patient);
        var result = await context.Patients.FirstOrDefaultAsync(p => p.Name.TheName == "Delete Me");

        // Assert
        Assert.Null(result);
    }
    /*
    [Fact]
    public async Task SearchPatientsByFiltersAsync_ReturnsCorrectPatients()
    {
        // Arrange
        var context = await GetInMemoryDbContextAsync();
        var repository = new PatientRepository(context);

        var patient = CreatePatientDataModel("Filtered Patient", "20241010108");
        context.Patients.Add(patient);
        await context.SaveChangesAsync();

        var criteria = new PatientFilterCriteriaDTO("Filtered Patient");
        
        
        // Act
        var result = await repository.SearchPatientsByFiltersAsync(criteria);

        // Assert
        Assert.Single(result);
        Assert.Equal("Filtered Patient", result.First().Name.TheName);
    }
    */
}


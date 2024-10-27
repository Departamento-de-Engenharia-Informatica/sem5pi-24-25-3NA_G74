using System;
using G74.DataModel;
using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.Mappers;
using Xunit;

namespace G74.Tests.Mappers;

public class PatientMapperTest
{
    [Fact]
    public void ToDTO_ValidPatient_ReturnsPatientDTO()
    {
        // Arrange
        var patient = new Patient(
            new Name("John Doe"),
            new MedicalRecordNumber("20241010101"),
            new DateOfBirth(1990, 10, 1),
            new Gender(Gender.GenderEnum.Male),
            new ContactInformation("913283295", new Email("john.doe@gmail.com")),
            new EmergencyContact("931231422")
        );

        // Act
        var result = PatientMapper.ToDTO(patient);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("John Doe", result.Name.TheName);
        Assert.Equal("Male", result.Gender);
        Assert.Equal(1990, result.DateOfBirth.YearOfBirth);
        Assert.Equal(10, result.DateOfBirth.MonthOfBirth);
        Assert.Equal(1, result.DateOfBirth.DayOfBirth);
        Assert.Equal("913283295", result.ContactInformation.PhoneNumber);
        Assert.Equal("john.doe@gmail.com", result.ContactInformation.EmailAddress.email);
        Assert.Equal("931231422", result.EmergencyContact._phoneNumber);
    }

    [Fact]
    public void ToDTO_NullPatient_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => PatientMapper.ToDTO(null));
    }

    [Fact]
    public void ToDataModel_ValidPatient_ReturnsPatientDataModel()
    {
        // Arrange
        var patient = new Patient(
            new Name("Jane Doe"),
            new MedicalRecordNumber("20241010102"),
            new DateOfBirth(1985, 5, 15),
            new Gender(Gender.GenderEnum.Female),
            new ContactInformation("912345678", new Email("jane.doe@gmail.com")),
            new EmergencyContact("912345679")
        );

        // Act
        var result = PatientMapper.ToDataModel(patient);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Jane Doe", result.Name.TheName);
        Assert.Equal("Female", result.Gender.GenderDescription);
        Assert.Equal(1985, result.DateOfBirth.YearOfBirth);
        Assert.Equal(5, result.DateOfBirth.MonthOfBirth);
        Assert.Equal(15, result.DateOfBirth.DayOfBirth);
        Assert.Equal("912345678", result.ContactInformation.PhoneNumber);
        Assert.Equal("jane.doe@gmail.com", result.ContactInformation.EmailAddress.email);
        Assert.Equal("912345679", result.EmergencyContact._phoneNumber);
    }

    [Fact]
    public void ToDataModel_NullPatient_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => PatientMapper.ToDataModel(null));
    }

    [Fact]
    public void FromDataModelToCreatePatientDto_ValidPatientDataModel_ReturnsCreatePatientDTO()
    {
        // Arrange
        
        var patient = new Patient(
            new Name("Alice Doe"),
            new MedicalRecordNumber("20241010105"),
            new DateOfBirth(1975, 7, 20),
            new Gender(Gender.GenderEnum.Female),
            new ContactInformation("919876543", new Email("alice.doe@gmail.com")),
            new EmergencyContact("919876544")
        );

        var patientDataModel = new PatientDataModel(patient);
        

        // Act
        var result = PatientMapper.FromDataModelToCreatePatientDto(patientDataModel);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Alice Doe", result.Name);
        Assert.Equal("Female", result.Gender);
        Assert.Equal(1975, result.DateOfBirth.YearOfBirth);
        Assert.Equal(7, result.DateOfBirth.MonthOfBirth);
        Assert.Equal(20, result.DateOfBirth.DayOfBirth);
        Assert.Equal("919876543", result.ContactInformation.PhoneNumber);
        Assert.Equal("alice.doe@gmail.com", result.ContactInformation.EmailAddress);
        Assert.Equal("919876544", result.EmergencyContact.PhoneNumber);
    }

    [Fact]
    public void FromDataModelToCreatePatientDto_NullPatientDataModel_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => PatientMapper.FromDataModelToCreatePatientDto(null));
    }
}
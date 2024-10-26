using System;
using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using Xunit;

namespace G74.Tests.Domain.Aggregates;

public class PatientTest
{
    private Name _validName = new Name("Alice Smith");
    private MedicalRecordNumber _validMedicalRecordNumber = new MedicalRecordNumber("20231000001");
    private DateOfBirth _validDateOfBirth = new DateOfBirth(1990, 5, 15);
    private Gender _validGender = new Gender(Gender.GenderEnum.Female);
    private ContactInformation _validContactInfo = new ContactInformation("912345678", new Email("alice@example.com"));
    private EmergencyContact _validEmergencyContact = new EmergencyContact("923456789");

    [Fact]
    public void Constructor_ValidInputs_CreatesPatient()
    {
        // Act
        var patient = new Patient(_validName, _validMedicalRecordNumber, _validDateOfBirth, _validGender, _validContactInfo, _validEmergencyContact);

        // Assert
        Assert.Equal(_validName, patient.Name);
        Assert.Equal(_validMedicalRecordNumber, patient.MedicalRecordNumber);
        Assert.Equal(_validDateOfBirth, patient.DateOfBirth);
        Assert.Equal(_validGender, patient.Gender);
        Assert.Equal(_validContactInfo, patient.ContactInformation);
        Assert.Equal(_validEmergencyContact, patient.EmergencyContact);
    }

    [Fact]
    public void Constructor_NullName_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(null, _validMedicalRecordNumber, _validDateOfBirth, _validGender, _validContactInfo, _validEmergencyContact));
    }

    [Fact]
    public void Constructor_NullMedicalRecordNumber_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(_validName, null, _validDateOfBirth, _validGender, _validContactInfo, _validEmergencyContact));
    }

    [Fact]
    public void Constructor_NullDateOfBirth_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(_validName, _validMedicalRecordNumber, null, _validGender, _validContactInfo, _validEmergencyContact));
    }

    [Fact]
    public void Constructor_NullGender_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(_validName, _validMedicalRecordNumber, _validDateOfBirth, null, _validContactInfo, _validEmergencyContact));
    }

    [Fact]
    public void Constructor_NullContactInformation_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(_validName, _validMedicalRecordNumber, _validDateOfBirth, _validGender, null, _validEmergencyContact));
    }

    [Fact]
    public void Constructor_NullEmergencyContact_ThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(_validName, _validMedicalRecordNumber, _validDateOfBirth, _validGender, _validContactInfo, null));
    }

    [Fact]
    public void Constructor_ValidInputs_SetsMedicalConditionToNull()
    {
        // Act
        var patient = new Patient(_validName, _validMedicalRecordNumber, _validDateOfBirth, _validGender, _validContactInfo, _validEmergencyContact);

        // Assert
        Assert.Null(patient.MedicalCondition);
    }
}
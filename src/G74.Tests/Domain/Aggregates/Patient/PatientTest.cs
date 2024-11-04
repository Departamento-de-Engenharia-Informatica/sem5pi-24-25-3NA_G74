using System;
using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using Xunit;

public class PatientTests
{
    [Fact]
    public void Constructor_CreatesPatient_WhenAllPropertiesAreValid()
    {
        // Arrange
        var name = new Name("John Doe");
        var medicalRecordNumber = new MedicalRecordNumber("202411000001");
        var dateOfBirth = new DateOfBirth(1990, 5, 15);
        var gender = new Gender(Gender.GenderEnum.Male);
        var contactInformation = new ContactInformation("912 345 678", new Email("john.doe@example.com"));
        var emergencyContact = new EmergencyContact("931 234 567", new Name("Jane Doe"));

        // Act
        var patient = new Patient(name, medicalRecordNumber, dateOfBirth, gender, contactInformation, emergencyContact);

        // Assert
        Assert.Equal(name, patient.Name);
        Assert.Equal(medicalRecordNumber, patient.MedicalRecordNumber);
        Assert.Equal(dateOfBirth, patient.DateOfBirth);
        Assert.Equal(gender, patient.Gender);
        Assert.Equal(contactInformation, patient.ContactInformation);
        Assert.Equal(emergencyContact, patient.EmergencyContact);
    }

    [Theory]
    [InlineData(null, "Medical Record Number", "Date of Birth", "Gender", "Contact Information", "Emergency Contact")]
    [InlineData("Name", null, "Date of Birth", "Gender", "Contact Information", "Emergency Contact")]
    [InlineData("Name", "Medical Record Number", null, "Gender", "Contact Information", "Emergency Contact")]
    [InlineData("Name", "Medical Record Number", "Date of Birth", null, "Contact Information", "Emergency Contact")]
    [InlineData("Name", "Medical Record Number", "Date of Birth", "Gender", null, "Emergency Contact")]
    [InlineData("Name", "Medical Record Number", "Date of Birth", "Gender", "Contact Information", null)]
    public void Constructor_ThrowsArgumentNullException_WhenAnyRequiredPropertyIsNull(
        string nameValue, string mrnValue, string dobValue, string genderValue, string contactInfoValue, string emergencyContactValue)
    {
        // Arrange
        Name? name = nameValue == null ? null : new Name("John Doe");
        MedicalRecordNumber? medicalRecordNumber = mrnValue == null ? null : new MedicalRecordNumber("202411000001");
        DateOfBirth? dateOfBirth = dobValue == null ? null : new DateOfBirth(1990, 5, 15);
        Gender? gender = genderValue == null ? null : new Gender(Gender.GenderEnum.Male);
        ContactInformation? contactInformation = contactInfoValue == null ? null : new ContactInformation("912 345 678", new Email("john.doe@example.com"));
        EmergencyContact? emergencyContact = emergencyContactValue == null ? null : new EmergencyContact("931 234 567", new Name("Jane Doe"));

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new Patient(name, medicalRecordNumber, dateOfBirth, gender, contactInformation, emergencyContact));
    }

    [Fact]
    public void UpdateName_UpdatesName_WhenValidNameIsProvided()
    {
        // Arrange
        var patient = CreateValidPatient();
        var newName = new Name("Jane Doe");

        // Act
        patient.UpdateName(newName);

        // Assert
        Assert.Equal(newName, patient.Name);
    }

    [Fact]
    public void UpdateName_ThrowsArgumentNullException_WhenNameIsNull()
    {
        // Arrange
        var patient = CreateValidPatient();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => patient.UpdateName(null));
    }

    [Fact]
    public void UpdateDateOfBirth_UpdatesDateOfBirth_WhenValidDateOfBirthIsProvided()
    {
        // Arrange
        var patient = CreateValidPatient();
        var newDateOfBirth = new DateOfBirth(1991, 6, 25);

        // Act
        patient.UpdateDateOfBirth(newDateOfBirth);

        // Assert
        Assert.Equal(newDateOfBirth, patient.DateOfBirth);
    }

    [Fact]
    public void UpdateDateOfBirth_ThrowsArgumentNullException_WhenDateOfBirthIsNull()
    {
        // Arrange
        var patient = CreateValidPatient();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => patient.UpdateDateOfBirth(null));
    }

    [Fact]
    public void UpdateContactInformation_UpdatesContactInformation_WhenValidContactInformationIsProvided()
    {
        // Arrange
        var patient = CreateValidPatient();
        var newContactInformation = new ContactInformation("915 678 123", new Email("jane.doe@example.com"));

        // Act
        patient.UpdateContactInformation(newContactInformation);

        // Assert
        Assert.Equal(newContactInformation, patient.ContactInformation);
    }

    [Fact]
    public void UpdateContactInformation_ThrowsArgumentNullException_WhenContactInformationIsNull()
    {
        // Arrange
        var patient = CreateValidPatient();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => patient.UpdateContactInformation(null));
    }

    [Fact]
    public void UpdateEmergencyContact_UpdatesEmergencyContact_WhenValidEmergencyContactIsProvided()
    {
        // Arrange
        var patient = CreateValidPatient();
        var newEmergencyContact = new EmergencyContact("932 000 111", new Name("Jake Doe"));

        // Act
        patient.UpdateEmergencyContact(newEmergencyContact);

        // Assert
        Assert.Equal(newEmergencyContact, patient.EmergencyContact);
    }

    [Fact]
    public void UpdateEmergencyContact_ThrowsArgumentNullException_WhenEmergencyContactIsNull()
    {
        // Arrange
        var patient = CreateValidPatient();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => patient.UpdateEmergencyContact(null));
    }

    [Fact]
    public void Equals_ReturnsTrue_WhenPatientsHaveSameProperties()
    {
        // Arrange
        var patient1 = CreateValidPatient();
        var patient2 = CreateValidPatient();

        // Act & Assert
        Assert.True(patient1.Equals(patient2));
    }

    [Fact]
    public void Equals_ReturnsFalse_WhenPatientsHaveDifferentProperties()
    {
        // Arrange
        var patient1 = CreateValidPatient();
        var patient2 = new Patient(
            new Name("Jane Doe"),
            new MedicalRecordNumber("202411000002"),
            new DateOfBirth(1991, 6, 25),
            new Gender(Gender.GenderEnum.Female),
            new ContactInformation("915 678 123", new Email("jane.doe@example.com")),
            new EmergencyContact("932 000 111", new Name("Jake Doe"))
        );

        // Act & Assert
        Assert.False(patient1.Equals(patient2));
    }

    [Fact]
    public void GetHashCode_ReturnsSameHash_WhenPatientsAreEqual()
    {
        // Arrange
        var patient1 = CreateValidPatient();
        var patient2 = CreateValidPatient();

        // Act & Assert
        Assert.Equal(patient1.GetHashCode(), patient2.GetHashCode());
    }

    [Fact]
    public void GetHashCode_ReturnsDifferentHash_WhenPatientsAreNotEqual()
    {
        // Arrange
        var patient1 = CreateValidPatient();
        var patient2 = new Patient(
            new Name("Jane Doe"),
            new MedicalRecordNumber("202411000002"),
            new DateOfBirth(1991, 6, 25),
            new Gender(Gender.GenderEnum.Female),
            new ContactInformation("915 678 123", new Email("jane.doe@example.com")),
            new EmergencyContact("932 000 111", new Name("Jake Doe"))
        );

        // Act & Assert
        Assert.NotEqual(patient1.GetHashCode(), patient2.GetHashCode());
    }

    // Helper method to create a valid patient instance
    private static Patient CreateValidPatient()
    {
        return new Patient(
            new Name("John Doe"),
            new MedicalRecordNumber("202411000001"),
            new DateOfBirth(1990, 5, 15),
            new Gender(Gender.GenderEnum.Male),
            new ContactInformation("912 345 678", new Email("john.doe@example.com")),
            new EmergencyContact("931 234 567", new Name("Jane Doe"))
        );
    }
}

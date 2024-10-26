using System.Threading.Tasks;
using G74.Domain.Builders;
using G74.Domain.DomainServices;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using Moq;
using Xunit;

namespace G74.Tests.Domain.Builders;

public class PatientBuilderTest
{
    private readonly Mock<IMedicalRecordNumberGenerator> _medicalRecordNumberGeneratorMock;
    private readonly Name _validName = new Name("Alice Smith");
    private readonly DateOfBirth _validDateOfBirth = new DateOfBirth(1990, 5, 15);
    private readonly Gender _validGender = new Gender(Gender.GenderEnum.Female);
    private readonly ContactInformation _validContactInformation = new ContactInformation("912345678", new Email("alice@example.com"));
    private readonly EmergencyContact _validEmergencyContact = new EmergencyContact("923456789");
    private readonly MedicalRecordNumber _generatedMedicalRecordNumber = new MedicalRecordNumber("20231000001");

    public PatientBuilderTest()
    {
        _medicalRecordNumberGeneratorMock = new Mock<IMedicalRecordNumberGenerator>();
        _medicalRecordNumberGeneratorMock
            .Setup(m => m.GenerateMedicalNumber())
            .ReturnsAsync(_generatedMedicalRecordNumber);
    }

    [Fact]
    public async Task Build_WithValidData_CreatesPatient()
    {
        // Arrange
        var builder = new PatientBuilder(
            _medicalRecordNumberGeneratorMock.Object,
            _validName,
            _validDateOfBirth,
            _validGender,
            _validContactInformation,
            _validEmergencyContact
        );

        // Act
        var patient = await builder.Build();

        // Assert
        Assert.Equal(_validName, patient.Name);
        Assert.Equal(_generatedMedicalRecordNumber, patient.MedicalRecordNumber);
        Assert.Equal(_validDateOfBirth, patient.DateOfBirth);
        Assert.Equal(_validGender, patient.Gender);
        Assert.Equal(_validContactInformation, patient.ContactInformation);
        Assert.Equal(_validEmergencyContact, patient.EmergencyContact);
        Assert.Null(patient.MedicalCondition);
        Assert.Null(patient.AppointmentHistory);
    }

    [Fact]
    public async Task Build_CallsMedicalRecordNumberGeneratorOnce()
    {
        // Arrange
        var builder = new PatientBuilder(
            _medicalRecordNumberGeneratorMock.Object,
            _validName,
            _validDateOfBirth,
            _validGender,
            _validContactInformation,
            _validEmergencyContact
        );

        // Act
        await builder.Build();

        // Assert
        _medicalRecordNumberGeneratorMock.Verify(m => m.GenerateMedicalNumber(), Times.Once);
    }

    [Fact]
    public async Task Build_WhenCalled_SetsMedicalRecordNumber()
    {
        // Arrange
        var builder = new PatientBuilder(
            _medicalRecordNumberGeneratorMock.Object,
            _validName,
            _validDateOfBirth,
            _validGender,
            _validContactInformation,
            _validEmergencyContact
        );

        // Act
        var patient = await builder.Build();

        // Assert
        Assert.Equal(_generatedMedicalRecordNumber, patient.MedicalRecordNumber);
    }
}
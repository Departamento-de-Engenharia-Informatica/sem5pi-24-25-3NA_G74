using System;
using System.Threading.Tasks;
using G74.Domain.DomainServices;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using Moq;
using Xunit;

namespace G74.Tests.Domain.DomainServices;

public class MedicalNumberGeneratorTest
{
    
    private const string MedicalRecordNumberValidationPattern = @"^\d{4}(0[1-9]|1[0-2])\d{6}$";
    
    [Fact]
    public async Task GenerateMedicalNumber_ReturnsValidMedicalRecordNumber()
    {
        // Arrange
        var mockMedicalRecordNumber = "202310000001"; // Example valid medical record number
        var medicalRecordNumberGeneratorMock = new Mock<IMedicalRecordNumberGenerator>();

        medicalRecordNumberGeneratorMock
            .Setup(m => m.GenerateMedicalNumber())
            .ReturnsAsync(new MedicalRecordNumber(mockMedicalRecordNumber));

        // Act
        var generatedMedicalRecordNumber = await medicalRecordNumberGeneratorMock.Object.GenerateMedicalNumber();

        // Assert
        Assert.Matches(MedicalRecordNumberValidationPattern, generatedMedicalRecordNumber.MedicalNumber);
    }

    [Theory]
    [InlineData("202313000001")]  // Invalid month (13)
    [InlineData("202300000001")]  // Invalid month (00)
    [InlineData("2023100000124")] // Too long
    [InlineData("20231")]        // Too short
    [InlineData("123456789012")] // Incorrect format
    public void Constructor_InvalidMedicalRecordNumber_ThrowsArgumentException(string invalidNumber)
    {
        // Act & Assert
        var exception = Assert.Throws<BusinessRuleValidationException>(() => new MedicalRecordNumber(invalidNumber));
        Assert.Equal("Invalid medical record number format. Expected format: YYYYMMnnnnnn.", exception.Message);
    }
    
}
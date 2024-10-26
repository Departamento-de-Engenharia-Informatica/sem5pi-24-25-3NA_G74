using System;
using G74.Domain.Value_Objects.Patient;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.Patient;

public class MedicalRecordNumberTest
{
    [Theory]
    [InlineData("20231000001")]  // Valid record for October 2023, first patient
    [InlineData("20231212345")]  // Valid record for December 2023, sequential 12345
    [InlineData("20230154321")]  // Valid record for January 2023, sequential 54321
    public void Constructor_ValidMedicalRecordNumber_CreatesInstance(string validNumber)
    {
        // Act
        var medicalRecordNumber = new MedicalRecordNumber(validNumber);

        // Assert
        Assert.Equal(validNumber, medicalRecordNumber.MedicalNumber);
    }

    [Theory]
    [InlineData("20231300001")]      // Invalid month (13)
    [InlineData("20230000001")]      // Invalid month (00)
    [InlineData("2023A100001")]      // Non-numeric character in month
    [InlineData("2023100000123")]    // Too long
    [InlineData("20231")]            // Too short
    [InlineData("202311234")]        // Missing digits in sequential part
    public void Constructor_InvalidMedicalRecordNumber_ThrowsArgumentException(string invalidNumber)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new MedicalRecordNumber(invalidNumber));
        Assert.Equal("Invalid medical record number format. Expected format: YYYYMMDDDDD.", exception.Message);
    }

    [Fact]
    public void ToString_ReturnsMedicalNumber()
    {
        // Arrange
        var validNumber = "20231000001";
        var medicalRecordNumber = new MedicalRecordNumber(validNumber);

        // Act
        var result = medicalRecordNumber.ToString();

        // Assert
        Assert.Equal(validNumber, result);
    }
}
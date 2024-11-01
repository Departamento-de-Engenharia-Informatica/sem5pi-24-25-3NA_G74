using System;
using G74.Domain.Value_Objects.Patient;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.Patient;

public class MedicalConditionTest
{
    [Theory]
    [InlineData("Diabetes")]
    [InlineData("Hypertension")]
    [InlineData("Allergy to penicillin")]
    public void Constructor_ValidDescription_CreatesMedicalCondition(string validDescription)
    {
        // Act
        var medicalCondition = new MedicalCondition(validDescription);

        // Assert
        Assert.Equal(validDescription, medicalCondition.Description);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")] // Only whitespace
    [InlineData(null)]   // Null value
    public void Constructor_InvalidDescription_ThrowsArgumentException(string invalidDescription)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new MedicalCondition(invalidDescription));
        Assert.Equal("Medical condition cannot be empty or spaces", exception.Message);
    }
}
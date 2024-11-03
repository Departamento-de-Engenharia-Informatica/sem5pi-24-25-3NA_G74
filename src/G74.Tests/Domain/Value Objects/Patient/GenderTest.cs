using System;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Tests.Domain.Value_Objects.Patient;

using Xunit;

public class GenderTest
{
    [Theory]
    [InlineData("Male", "Male")]
    [InlineData("Female", "Female")]
    [InlineData("Other", "Other")]
    [InlineData("male", "Male")]     // Case-insensitive check
    [InlineData("female", "Female")] // Case-insensitive check
    [InlineData("other", "Other")]   // Case-insensitive check
    public void FromString_ValidString_ReturnsGender(string input, string expected)
    {
        // Act
        var gender = Gender.FromString(input);

        // Assert
        Assert.Equal(expected, gender.GenderDescription);
    }

    [Theory]
    [InlineData("Unknown")]  // Invalid value
    [InlineData("NotAGender")]  // Invalid string
    [InlineData("")]         // Empty string
    [InlineData(null)]       // Null string
    public void FromString_InvalidString_ThrowsArgumentException(string input)
    {
        // Act & Assert
        var exception = Assert.Throws<BusinessRuleValidationException>(() => Gender.FromString(input));
        Assert.Equal("Invalid gender", exception.Message);
    }

    [Theory]
    [InlineData(Gender.GenderEnum.Male, "Male")]
    [InlineData(Gender.GenderEnum.Female, "Female")]
    [InlineData(Gender.GenderEnum.Other, "Other")]
    public void Constructor_ValidEnum_SetsGenderDescription(Gender.GenderEnum genderEnum, string expectedDescription)
    {
        // Act
        var gender = new Gender(genderEnum);

        // Assert
        Assert.Equal(expectedDescription, gender.GenderDescription);
    }

    [Theory]
    [InlineData(Gender.GenderEnum.Male, "Male")]
    [InlineData(Gender.GenderEnum.Female, "Female")]
    [InlineData(Gender.GenderEnum.Other, "Other")]
    public void ToString_ReturnsCorrectDescription(Gender.GenderEnum genderEnum, string expectedDescription)
    {
        // Act
        var gender = new Gender(genderEnum);

        // Assert
        Assert.Equal(expectedDescription, gender.ToString());
    }
}
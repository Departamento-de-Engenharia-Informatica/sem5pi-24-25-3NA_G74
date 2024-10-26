using System;
using G74.Domain.Value_Objects.Patient;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.Patient;

public class EmergencyContactTest
{
    private const string ValidPhoneNumber = "912345678";

    [Fact]
    public void Constructor_ValidPhoneNumber_CreatesEmergencyContact()
    {
        // Act
        var emergencyContact = new EmergencyContact(ValidPhoneNumber);

        // Assert
        Assert.Equal(ValidPhoneNumber, emergencyContact._phoneNumber);
    }

    [Theory]
    [InlineData("12345678")]               // Too short
    [InlineData("9876543210")]             // Too long
    [InlineData("812345678")]              // Invalid starting digit
    [InlineData("+351 812345678")]         // Invalid Portuguese prefix
    public void Constructor_InvalidPhoneNumber_ThrowsArgumentException(string invalidPhoneNumber)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new EmergencyContact(invalidPhoneNumber));
        Assert.Equal("Invalid portuguese phone number", exception.Message);
    }

    [Fact]
    public void CopyConstructor_CreatesCopy()
    {
        // Arrange
        var originalContact = new EmergencyContact(ValidPhoneNumber);

        // Act
        var copyContact = new EmergencyContact(originalContact);

        // Assert
        Assert.Equal(originalContact._phoneNumber, copyContact._phoneNumber);
    }

    [Theory]
    [InlineData("912345678")]
    [InlineData("+351 923456789")]
    public void FromString_ValidPhoneNumber_ReturnsEmergencyContact(string input)
    {
        // Act
        var emergencyContact = EmergencyContact.FromString(input);

        
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")] // Only spaces
    [InlineData(null)]  // Null value
    public void FromString_InvalidPhoneNumber_ThrowsArgumentNullException(string input)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentNullException>(() => EmergencyContact.FromString(input));
        
    }

    [Fact]
    public void ToString_ReturnsPhoneNumber()
    {
        // Arrange
        var emergencyContact = new EmergencyContact(ValidPhoneNumber);

        // Act
        var result = emergencyContact.ToString();

        // Assert
        Assert.Equal(ValidPhoneNumber, result);
    }
}
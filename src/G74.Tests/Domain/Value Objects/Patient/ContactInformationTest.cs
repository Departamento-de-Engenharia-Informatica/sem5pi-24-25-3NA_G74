using System;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.User;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.Patient;

public class ContactInformationTest
{
     private const string ValidPhoneNumber = "912345678";
    private const string ValidEmailAddress = "test@example.com";

    [Fact]
    public void Constructor_ValidContactInformation_CreatesContactInformation()
    {
        // Arrange
        var email = new Email(ValidEmailAddress);

        // Act
        var contactInfo = new ContactInformation(ValidPhoneNumber, email);

        // Assert
        Assert.Equal(ValidPhoneNumber, contactInfo.PhoneNumber);
        Assert.Equal(email, contactInfo.EmailAddress);
    }

    [Theory]
    [InlineData("12345678")]               // Too short
    [InlineData("9876543210")]             // Too long
    [InlineData("812345678")]              // Invalid starting digit
    [InlineData("+351 812345678")]         // Invalid Portuguese prefix
    public void Constructor_InvalidPhoneNumber_ThrowsArgumentException(string invalidPhoneNumber)
    {
        // Arrange
        var email = new Email(ValidEmailAddress);

        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new ContactInformation(invalidPhoneNumber, email));
        Assert.Equal("Invalid portuguese phone number", exception.Message);
    }

    [Fact]
    public void Constructor_ValidEmail_SetsEmailAddress()
    {
        // Arrange
        var email = new Email(ValidEmailAddress);

        // Act
        var contactInfo = new ContactInformation(ValidPhoneNumber, email);

        // Assert
        Assert.Equal(ValidEmailAddress, contactInfo.Email);
    }

    [Fact]
    public void CopyConstructor_CreatesCopy()
    {
        // Arrange
        var email = new Email(ValidEmailAddress);
        var originalContactInfo = new ContactInformation(ValidPhoneNumber, email);

        // Act
        var copyContactInfo = new ContactInformation(originalContactInfo);

        // Assert
        Assert.Equal(originalContactInfo.PhoneNumber, copyContactInfo.PhoneNumber);
        Assert.Equal(originalContactInfo.EmailAddress, copyContactInfo.EmailAddress);
    }

    [Theory]
    [InlineData("912345678;test@example.com", "912345678", "test@example.com")]
    [InlineData("+351 923456789;another@example.com", "+351 923456789", "another@example.com")]
    public void FromString_ValidString_ReturnsContactInformation(string input, string expectedPhone, string expectedEmail)
    {
        // Act
        var contactInfo = ContactInformation.FromString(input);

        // Assert
        Assert.Equal(expectedPhone, contactInfo.PhoneNumber);
        Assert.Equal(expectedEmail, contactInfo.Email);
    }

    [Theory]
    [InlineData("invalidString")]
    [InlineData("912345678")]
    public void FromString_InvalidString_ThrowsFormatException(string input)
    {
        // Act & Assert
        Assert.Throws<FormatException>(() => ContactInformation.FromString(input));
    }

    [Theory]
    [InlineData("912345678;invalid-email")]
    public void FromString_Invalid_Email_ThrowsArgumentException(string input)
    {
        Assert.Throws<ArgumentException>(() => ContactInformation.FromString(input));
    }

    [Fact]
    public void ToString_ReturnsFormattedContactInformation()
    {
        // Arrange
        var email = new Email(ValidEmailAddress);
        var contactInfo = new ContactInformation(ValidPhoneNumber, email);
        
        // Act
        var result = contactInfo.ToString();

        // Assert
        Assert.Equal($"{ValidPhoneNumber};{ValidEmailAddress}", result);
    }
}
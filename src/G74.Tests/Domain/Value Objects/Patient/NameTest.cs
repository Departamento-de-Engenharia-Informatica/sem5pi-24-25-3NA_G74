using System;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Tests.Domain.Value_Objects.Patient;

using Xunit;

public class NameTest
{
    [Theory]
    [InlineData("Alice")]
    [InlineData("Bob")]
    [InlineData(" John Doe ")] // Whitespace around valid name
    public void Constructor_ValidName_CreatesName(string input)
    {
        // Act
        var name = new Name(input);

        // Assert
        Assert.Equal(input.Trim(), name.Value);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")] // Only spaces
    [InlineData(null)]   // Null value
    public void Constructor_InvalidName_ThrowsArgumentException(string input)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new Name(input));
        Assert.Equal("Name cannot be empty or spaces", exception.Message);
    }

    [Theory]
    [InlineData("Alice")]
    [InlineData(" Bob ")] // Leading and trailing whitespace
    [InlineData("Charlie")]
    public void FromString_ValidName_ReturnsName(string input)
    {
        // Act
        var name = Name.FromString(input);

        // Assert
        Assert.Equal(input.Trim(), name.Value);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")] // Only spaces
    [InlineData(null)]   // Null value
    public void FromString_InvalidName_ThrowsArgumentException(string input)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => Name.FromString(input));
        Assert.Equal("Name cannot be empty or spaces.", exception.Message);
    }

    [Theory]
    [InlineData("Alice")]
    [InlineData(" Bob ")] // Leading and trailing whitespace
    [InlineData("Charlie")]
    public void ToString_ReturnsCorrectName(string input)
    {
        // Arrange
        var name = new Name(input);

        // Act
        var result = name.ToString();

        // Assert
        Assert.Equal(input.Trim(), result);
    }
}
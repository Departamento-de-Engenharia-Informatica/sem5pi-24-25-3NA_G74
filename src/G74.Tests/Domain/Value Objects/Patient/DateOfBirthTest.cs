using System;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Tests.Domain.Value_Objects.Patient;
using Xunit;

public class DateOfBirthTest
{
    [Theory]
    [InlineData(1990, 1, 15)]
    [InlineData(2000, 12, 31)]
    [InlineData(1985, 5, 10)]
    public void Constructor_ValidDate_CreatesDateOfBirth(int year, int month, int day)
    {
        // Arrange & Act
        var dateOfBirth = new DateOfBirth(year, month, day);

        // Assert
        Assert.Equal(year, dateOfBirth.YearOfBirth);
        Assert.Equal(month, dateOfBirth.MonthOfBirth);
        Assert.Equal(day, dateOfBirth.DayOfBirth);
    }

    [Fact]
    public void Constructor_DateInFuture_ThrowsArgumentException()
    {

        int year = DateTime.Now.Year + 1;
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new DateOfBirth(year, 1, 1));
        Assert.Equal("Date of birth cannot be in the future", exception.Message);
    }

    [Theory]
    [InlineData(2000, 2, 30)] // Invalid date
    [InlineData(2010, 13, 5)]  // Invalid month
    [InlineData(2020, 4, 31)]  // Invalid day for month
    public void Constructor_InvalidDate_ThrowsArgumentException(int year, int month, int day)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new DateOfBirth(year, month, day));
        Assert.Equal("Invalid date provided", exception.Message);
    }

    [Fact]
    public void CopyConstructor_ValidDate_CreatesCopy()
    {
        // Arrange
        var originalDateOfBirth = new DateOfBirth(1980, 3, 25);

        // Act
        var copy = new DateOfBirth(originalDateOfBirth);

        // Assert
        Assert.Equal(originalDateOfBirth.YearOfBirth, copy.YearOfBirth);
        Assert.Equal(originalDateOfBirth.MonthOfBirth, copy.MonthOfBirth);
        Assert.Equal(originalDateOfBirth.DayOfBirth, copy.DayOfBirth);
    }

    [Theory]
    [InlineData("1980-12-15", 1980, 12, 15)]
    [InlineData("2005-01-01", 2005, 1, 1)]
    public void FromString_ValidString_ReturnsDateOfBirth(string input, int expectedYear, int expectedMonth, int expectedDay)
    {
        // Act
        var dateOfBirth = DateOfBirth.FromString(input);

        // Assert
        Assert.Equal(expectedYear, dateOfBirth.YearOfBirth);
        Assert.Equal(expectedMonth, dateOfBirth.MonthOfBirth);
        Assert.Equal(expectedDay, dateOfBirth.DayOfBirth);
    }

    [Theory]
    [InlineData("2000-13-01")] // Invalid month
    [InlineData("2020-02-30")] // Invalid day
    public void FromString_InvalidDates_ThrowsArgumentException(string input)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => DateOfBirth.FromString(input));
    }
    
    [Theory]
    [InlineData("abcd-ef-gh")]  // Non-numeric format
    public void FromString_InvalidString_ThrowsFormatException(string input)
    {
        // Act & Assert
        Assert.Throws<FormatException>(() => DateOfBirth.FromString(input));
    }

    [Theory]
    [InlineData(1995, 11, 5, "1995-11-05")]
    [InlineData(2001, 6, 9, "2001-06-09")]
    public void ToFormattedDateOfBirthStr_ReturnsCorrectFormat(int year, int month, int day, string expected)
    {
        // Arrange
        var dateOfBirth = new DateOfBirth(year, month, day);

        // Act
        var result = dateOfBirth.ToFormattedDateOfBirthStr();

        // Assert
        Assert.Equal(expected, result);
    }
}
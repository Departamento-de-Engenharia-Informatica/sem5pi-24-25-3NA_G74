using System;
using G74.Domain.Value_Objects.Patient;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.Patient;

public class DeletionInformationTest
{
    [Fact]
    public void Constructor_IsMarkedForDeletionWithPositiveTimeToDeletion_SetsDateToBeDeleted()
    {
        // Arrange
        var timeToDeletion = TimeSpan.FromDays(1);

        // Act
        var deletionInfo = new DeletionInformation(true, timeToDeletion);

        // Assert
        Assert.True(deletionInfo.IsMarkedForDeletion);
        Assert.NotNull(deletionInfo.DateToBeDeleted);
        Assert.True(deletionInfo.DateToBeDeleted > DateTime.UtcNow);
    }

    [Fact]
    public void Constructor_IsMarkedForDeletionWithZeroTimeToDeletion_SetsDateToBeDeletedAsCurrentUtc()
    {
        // Arrange
        var timeToDeletion = TimeSpan.Zero;

        // Act
        var deletionInfo = new DeletionInformation(true, timeToDeletion);

        // Assert
        Assert.True(deletionInfo.IsMarkedForDeletion);
        Assert.NotNull(deletionInfo.DateToBeDeleted);
        Assert.Equal(DateTime.UtcNow.Date, deletionInfo.DateToBeDeleted?.Date);
    }

    [Fact]
    public void Constructor_NotMarkedForDeletion_SetsDateToBeDeletedAsNull()
    {
        // Act
        var deletionInfo = new DeletionInformation(false, TimeSpan.FromDays(1));

        // Assert
        Assert.False(deletionInfo.IsMarkedForDeletion);
        Assert.Null(deletionInfo.DateToBeDeleted);
    }

    [Fact]
    public void Constructor_NegativeTimeToDeletion_ThrowsArgumentException()
    {
        // Arrange
        var timeToDeletion = TimeSpan.FromDays(-1);

        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new DeletionInformation(true, timeToDeletion));
        Assert.Equal("Time to deletion must be positive when marked for deletion.", exception.Message);
    }

    [Fact]
    public void Equals_SameMarkedForDeletionAndDate_ReturnsTrue()
    {
        // Arrange
        var deletionInfo1 = new DeletionInformation(true, TimeSpan.FromHours(2));
        var deletionInfo2 = new DeletionInformation(true, TimeSpan.FromHours(2));

        // Act & Assert
        Assert.True(deletionInfo1.Equals(deletionInfo2));
    }

    [Fact]
    public void Equals_DifferentMarkedForDeletion_ReturnsFalse()
    {
        // Arrange
        var deletionInfo1 = new DeletionInformation(true, TimeSpan.FromHours(2));
        var deletionInfo2 = new DeletionInformation(false, TimeSpan.FromHours(2));

        // Act & Assert
        Assert.False(deletionInfo1.Equals(deletionInfo2));
    }

    [Fact]
    public void Equals_DifferentDateToBeDeleted_ReturnsFalse()
    {
        // Arrange
        var deletionInfo1 = new DeletionInformation(true, TimeSpan.FromHours(1));
        var deletionInfo2 = new DeletionInformation(true, TimeSpan.FromHours(2));

        // Act & Assert
        Assert.False(deletionInfo1.Equals(deletionInfo2));
    }
    
    [Fact]
    public void ToString_NotMarkedForDeletion_ReturnsNotMarkedMessage()
    {
        // Act
        var deletionInfo = new DeletionInformation(false, TimeSpan.FromHours(1));

        // Assert
        Assert.Equal("Not marked for deletion", deletionInfo.ToString());
    }

    [Fact]
    public void ToString_MarkedForDeletion_ReturnsFormattedDate()
    {
        // Arrange
        var timeToDeletion = TimeSpan.FromDays(1);
        var deletionInfo = new DeletionInformation(true, timeToDeletion);

        // Act
        var result = deletionInfo.ToString();

        // Assert
        Assert.Contains("Marked for deletion on", result);
    }
}
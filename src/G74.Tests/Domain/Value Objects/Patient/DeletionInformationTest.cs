using System;
using G74.Domain.Value_Objects.Patient;
using Xunit;

namespace G74.Tests.Domain.Value_Objects.Patient;

public class DeletionInformationTest
{
    [Theory]
    [InlineData(true, 10)]    // Deletion enabled with 10 minutes
    [InlineData(true, 0)]     // Deletion enabled with zero timespan
    [InlineData(false, 10)]   // Deletion disabled
    public void Constructor_SetsToDeleteAndDateToBeDeleted(bool toDelete, int minutes)
    {
        // Arrange
        var timeToDeletion = TimeSpan.FromMinutes(minutes);

        // Act
        var deletionInfo = new DeletionInformation(toDelete, timeToDeletion);

        // Assert
        Assert.Equal(toDelete, deletionInfo.ToDelete);
        if (toDelete)
        {
            Assert.NotNull(deletionInfo.DateToBeDeleted);
            Assert.InRange(deletionInfo.DateToBeDeleted.Value, DateTime.Now.AddMinutes(minutes - 1), DateTime.Now.AddMinutes(minutes + 1));
        }
        else
        {
            Assert.Null(deletionInfo.DateToBeDeleted);
        }
    }

    [Fact]
    public void Constructor_WithDeletionEnabled_SetsDateToBeDeletedCorrectly()
    {
        // Arrange
        var timeToDeletion = TimeSpan.FromHours(1);
        var expectedDateToBeDeleted = DateTime.Now.Add(timeToDeletion);

        // Act
        var deletionInfo = new DeletionInformation(true, timeToDeletion);

        // Assert
        Assert.True(deletionInfo.ToDelete);
        Assert.NotNull(deletionInfo.DateToBeDeleted);
        Assert.InRange(deletionInfo.DateToBeDeleted.Value, expectedDateToBeDeleted.AddSeconds(-1), expectedDateToBeDeleted.AddSeconds(1));
    }

    [Fact]
    public void Constructor_WithDeletionDisabled_DoesNotSetDateToBeDeleted()
    {
        // Arrange
        var timeToDeletion = TimeSpan.FromHours(1);

        // Act
        var deletionInfo = new DeletionInformation(false, timeToDeletion);

        // Assert
        Assert.False(deletionInfo.ToDelete);
        Assert.Null(deletionInfo.DateToBeDeleted);
    }
}
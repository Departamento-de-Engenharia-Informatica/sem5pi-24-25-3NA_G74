using System;
using System.Threading.Tasks;
using G74.Adapters.Repositories;
using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.Infrastructure;
using JetBrains.Annotations;
using Microsoft.EntityFrameworkCore;
using Xunit;
using System.Linq;

namespace G74.Tests.Adapters.Repositories;

[TestSubject(typeof(StaffRepository))]
public class StaffRepositoryTest
{
    private readonly DbContextOptions<BackofficeAppDbContext> _options;
    private readonly Staff _sampleStaff;

    public StaffRepositoryTest()
    {
        _options = new DbContextOptionsBuilder<BackofficeAppDbContext>()
            .UseInMemoryDatabase(databaseName: "TestDatabase")
            .Options;

        // Create sample staff for testing
        _sampleStaff = Staff.Create(
            123456,
            "John Doe",
            "912345678",
            "john.doe@example.com",
            "Cardiology",
            "Active",
            "600-700"
        );

        // Clean database before each test
        using var context = new BackofficeAppDbContext(_options);
        context.Database.EnsureDeleted();
        context.Database.EnsureCreated();
    }

    [Fact]
    public async Task Add_ValidStaff_ReturnsAddedStaff()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);

        // Act
        var result = await repository.Add(_sampleStaff);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(_sampleStaff.LicenceNumber.Value, result.LicenceNumber.Value);
        Assert.Equal(_sampleStaff.Name.Value, result.Name.Value);
        Assert.Equal(_sampleStaff.PhoneNumber.Value, result.PhoneNumber.Value);
        Assert.Equal(_sampleStaff.ContactEmail.email, result.ContactEmail.email);
        Assert.Equal(_sampleStaff.StaffSpecialization.Value, result.StaffSpecialization.Value);
        Assert.Equal(_sampleStaff.Status.Value, result.Status.Value);
    }

    [Fact]
    public async Task GetStaffAsync_WithExistingStaff_ReturnsAllStaff()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);
        await repository.Add(_sampleStaff);

        // Act
        var result = await repository.GetStaffAsync();

        // Assert
        var staffList = result.ToList();
        Assert.Single(staffList);
        Assert.Equal(_sampleStaff.LicenceNumber.Value, staffList[0].LicenceNumber.Value);
        Assert.Equal(_sampleStaff.Name.Value, staffList[0].Name.Value);
    }

    [Fact]
    public async Task GetByLicenceNumber_ExistingStaff_ReturnsStaff()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);
        await repository.Add(_sampleStaff);

        // Act
        var result = await repository.GetByLicenceNumber(new LicenceNumber(123456));

        // Assert
        Assert.NotNull(result);
        Assert.Equal(_sampleStaff.LicenceNumber.Value, result.LicenceNumber.Value);
        Assert.Equal(_sampleStaff.Name.Value, result.Name.Value);
    }

    [Fact]
    public async Task GetByLicenceNumber_NonExistingStaff_ReturnsNull()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);

        // Act
        var result = await repository.GetByLicenceNumber(new LicenceNumber(999999));

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task Update_ExistingStaff_ReturnsUpdatedStaff()
    {
        // Arrange
        using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);
        await repository.Add(_sampleStaff);

        var updatedStaff = Staff.Create(
            123456,
            "John Updated",
            "912345678",
            "john.updated@example.com",
            "Neurology",
            "Active",
            "700-800"
        );

        // Act
        var result = await repository.Update(new LicenceNumber(123456), updatedStaff);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(updatedStaff.Name.Value, result.Name.Value);
        Assert.Equal(updatedStaff.ContactEmail.email, result.ContactEmail.email);
        Assert.Equal(updatedStaff.StaffSpecialization.Value, result.StaffSpecialization.Value);
    }

    [Fact]
    public async Task Update_NonExistingStaff_ReturnsNull()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);

        // Act
        var result = await repository.Update(new LicenceNumber(999999), _sampleStaff);

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task UpdateStatus_ExistingStaff_ReturnsUpdatedStaff()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);
        await repository.Add(_sampleStaff);

        var deactivatedStaff = Staff.Create(
            123456,
            "John Doe",
            "912345678",
            "john.doe@example.com",
            "Cardiology",
            "Inactive",
            "600-700"
        );

        // Act
        var result = await repository.UpdateStatus(new LicenceNumber(123456), deactivatedStaff);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Inactive", result.Status.Value);
    }

    [Fact]
    public async Task UpdateStatus_NonExistingStaff_ThrowsException()
    {
        // Arrange
        await using var context = new BackofficeAppDbContext(_options);
        var repository = new StaffRepository(context);

        // Act & Assert
        await Assert.ThrowsAsync<Exception>(() => 
            repository.UpdateStatus(new LicenceNumber(999999), _sampleStaff)
        );
    }
}
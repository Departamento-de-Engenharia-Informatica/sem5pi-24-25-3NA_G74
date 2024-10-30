using System;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;
using JetBrains.Annotations;
using Xunit;

namespace G74.Tests.Domain.Aggregates.Staff;

[TestSubject(typeof(G74.Domain.Aggregates.Staff.Staff))]
public class StaffTest
{
    private readonly LicenseNumber _validLicenseNumber;
    private readonly Name _validName;
    private readonly PhoneNumber _validPhone;
    private readonly Email _validEmail;
    private readonly StaffSpecialization _validSpecialization;
    private readonly Status _activeStatus;

    public StaffTest()
    {
        // Initialize valid test data for reuse across tests
        _validLicenseNumber = new LicenseNumber("12345");
        _validName = new Name("John Doe");
        _validPhone = new PhoneNumber("+1234567890");
        _validEmail = new Email("john.doe@example.com");
        _validSpecialization = new StaffSpecialization("General");
        _activeStatus = new Status("active");
    }

    // [Fact]
    // public void CreateStaff_WithValidData_ShouldCreateStaffInstance()
    // {
    //     // Act
    //     var staff = new G74.Domain.Aggregates.Staff.Staff(
    //         _validLicenseNumber,
    //         _validName,
    //         _validPhone,
    //         _validEmail,
    //         _validSpecialization,
    //         _activeStatus
    //     );
    //
    //     // Assert
    //     Assert.NotNull(staff);
    //     Assert.Equal(_validLicenseNumber, staff.LicenseNumber);
    //     Assert.Equal(_validName, staff.Name);
    //     Assert.Equal(_validPhone, staff.PhoneNumber);
    //     Assert.Equal(_validEmail, staff.ContactEmail);
    //     Assert.Equal(_validSpecialization, staff.StaffSpecialization);
    //     Assert.Equal(_activeStatus, staff.Status);
    // }

    // [Fact]
    // public void CreateStaff_WithNullLicenseNumber_ShouldThrowArgumentNullException()
    // {
    //     // Act & Assert
    //     Assert.Throws<ArgumentNullException>(() => new G74.Domain.Aggregates.Staff.Staff(
    //         null,
    //         _validName,
    //         _validPhone,
    //         _validEmail,
    //         _validSpecialization,
    //         _activeStatus
    //     ));
    // }

    // [Theory]
    // [InlineData(null)]
    // [InlineData("")]
    // [InlineData(" ")]
    // public void CreateStaff_WithInvalidName_ShouldThrowBusinessRuleValidationException(string invalidName)
    // {
    //     // Arrange
    //     var invalidNameValue = new Name(invalidName);
    //
    //     // Act & Assert
    //     Assert.Throws<ArgumentNullException>(() => new G74.Domain.Aggregates.Staff.Staff(
    //         _validLicenseNumber,
    //         null,
    //         _validPhone,
    //         _validEmail,
    //         _validSpecialization,
    //         _activeStatus
    //     ));
    // }

    [Fact]
    public void Deactivate_WhenStaffIsActive_ShouldChangeStatusToDeactivated()
    {
        // Arrange
        var staff = new G74.Domain.Aggregates.Staff.Staff(
            _validLicenseNumber,
            _validName,
            _validPhone,
            _validEmail,
            _validSpecialization,
            _activeStatus
        );

        // Act
        staff.Deactivate();

        // Assert
        Assert.Equal("deactivated", staff.Status.Value);
    }

    [Fact]
    public void Deactivate_WhenStaffIsAlreadyDeactivated_ShouldKeepDeactivatedStatus()
    {
        // Arrange
        var deactivatedStatus = new Status("deactivated");
        var staff = new G74.Domain.Aggregates.Staff.Staff(
            _validLicenseNumber,
            _validName,
            _validPhone,
            _validEmail,
            _validSpecialization,
            deactivatedStatus
        );

        // Act
        staff.Deactivate();

        // Assert
        Assert.Equal("deactivated", staff.Status.Value);
    }
}
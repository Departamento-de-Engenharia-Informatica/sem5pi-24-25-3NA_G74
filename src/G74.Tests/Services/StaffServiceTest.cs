using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using G74.Domain.Aggregates.Staff;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Services;
using JetBrains.Annotations;
using Moq;
using Xunit;

namespace G74.Tests.Services;

[TestSubject(typeof(StaffService))]
public class StaffServiceTest
{
    private readonly Mock<IStaffRepository> _mockStaffRepository;
    private readonly StaffService _staffService;
    private readonly Staff _validStaff;
    private readonly StaffDto _validStaffDto;

    public StaffServiceTest()
    {
        _mockStaffRepository = new Mock<IStaffRepository>();
        // _staffService = new StaffService(_mockStaffRepository.Object);

        // // Create valid test data
        // _validStaff = new Staff(
        //     new LicenseNumber("12345"),
        //     new Name("John Smith"),
        //     new PhoneNumber("+1234567890"),
        //     new Email("john.Smith@example.com"),
        //     new StaffSpecialization("Doctor"),
        //     new Status("active")
        // );

        // _validStaffDto = new StaffDto(
        //     "12345",
        //     "John Smith",
        //     "+1234567890",
        //     "john.Smith@example.com",
        //     "Doctor",
        //     "active"
        // );
    }

    // [Fact]
    // public async Task GetAll_ReturnsAllStaff()
    // {
    //     // Arrange
    //     var staffList = new List<Staff> { _validStaff, _validStaff }; // Creating two staff members for testing
    //     _mockStaffRepository.Setup(repo => repo.GetStaffAsync())
    //         .ReturnsAsync(staffList);
    //
    //     // Act
    //     IEnumerable<StaffDto> result = await _staffService.GetAll();
    //
    //     // Assert
    //     Assert.NotNull(result);
    //     var staffDtos = result.ToList();
    //     Assert.Equal(2, staffDtos.Count); // Verify we got both staff members
    //     Assert.All(staffDtos, dto => 
    //     {
    //         Assert.Equal(_validStaff.LicenseNumber.Value, dto.LicenseNumber);
    //         Assert.Equal(_validStaff.Name.Value, dto.Name);
    //         Assert.Equal(_validStaff.PhoneNumber.Value, dto.PhoneNumber);
    //         Assert.Equal(_validStaff.ContactEmail.email, dto.ContactEmail);
    //         Assert.Equal(_validStaff.StaffSpecialization.Value, dto.StaffSpecialization);
    //         Assert.Equal(_validStaff.Status.Value, dto.Status);
    //     });
    // }
    //
    // [Fact]
    // public async Task GetByLicenseNumber_WithExistingLicense_ReturnsStaffDto()
    // {
    //     // Arrange
    //     _mockStaffRepository.Setup(repo => repo.GetByLicenseNumberAsync(_validStaff.LicenseNumber.Value))
    //         .ReturnsAsync(_validStaff);
    //
    //     // Act
    //     var result = await _staffService.GetByLicenseNumber(_validStaff.LicenseNumber.Value);
    //
    //     // Assert
    //     Assert.NotNull(result);
    //     Assert.Equal(_validStaff.LicenseNumber.Value, result.LicenseNumber);
    // }
    //
    // [Fact]
    // public async Task GetByLicenseNumber_WithNonExistingLicense_ReturnsNull()
    // {
    //     // Arrange
    //     _mockStaffRepository.Setup(repo => repo.GetByLicenseNumberAsync("nonexistent"))
    //         .ReturnsAsync((Staff)null);
    //
    //     // Act
    //     var result = await _staffService.GetByLicenseNumber("nonexistent");
    //
    //     // Assert
    //     Assert.Null(result);
    // }
    //
    // [Fact]
    // public async Task Add_WithNewStaff_ReturnsCreatedStaffDto()
    // {
    //     // Arrange
    //     _mockStaffRepository.Setup(repo => repo.StaffExists(_validStaffDto.LicenseNumber))
    //         .ReturnsAsync(false);
    //     _mockStaffRepository.Setup(repo => repo.Add(It.IsAny<Staff>()))
    //         .ReturnsAsync(_validStaff);
    //
    //     // Act
    //     var result = await _staffService.Add(_validStaffDto);
    //
    //     // Assert
    //     Assert.NotNull(result);
    //     Assert.Equal(_validStaffDto.LicenseNumber, result.LicenseNumber);
    //     _mockStaffRepository.Verify(repo => repo.Add(It.IsAny<Staff>()), Times.Once);
    // }
    //
    // [Fact]
    // public async Task Add_WithExistingLicenseNumber_ThrowsException()
    // {
    //     // Arrange
    //     _mockStaffRepository.Setup(repo => repo.StaffExists(_validStaffDto.LicenseNumber))
    //         .ReturnsAsync(true);
    //
    //     // Act & Assert
    //     await Assert.ThrowsAsync<Exception>(async () => 
    //         await _staffService.Add(_validStaffDto));
    // }
    //
    // [Fact]
    // public async Task Update_WithExistingStaff_ReturnsUpdatedStaffDto()
    // {
    //     // Arrange
    //     var updatedStaff = new Staff(
    //         new LicenseNumber(_validStaff.LicenseNumber.Value),
    //         new Name("Jane Smith"),
    //         _validStaff.PhoneNumber,
    //         _validStaff.ContactEmail,
    //         _validStaff.StaffSpecialization,
    //         _validStaff.Status
    //     );
    //
    //     _mockStaffRepository.Setup(repo => repo.Update(_validStaff.LicenseNumber.Value, It.IsAny<Staff>()))
    //         .ReturnsAsync(updatedStaff);
    //
    //     var updateDto = new StaffDto(
    //         _validStaffDto.LicenseNumber,
    //         "Jane Smith",
    //         _validStaffDto.PhoneNumber,
    //         _validStaffDto.ContactEmail,
    //         _validStaffDto.StaffSpecialization,
    //         _validStaffDto.Status
    //     );
    //
    //     // Act
    //     var result = await _staffService.Update(_validStaff.LicenseNumber.Value, updateDto);
    //
    //     // Assert
    //     Assert.NotNull(result);
    //     Assert.Equal("Jane Smith", result.Name);
    //     _mockStaffRepository.Verify(repo => repo.Update(_validStaff.LicenseNumber.Value, It.IsAny<Staff>()), Times.Once);
    // }
    //
    // [Fact]
    // public async Task Deactivate_WithExistingStaff_ReturnsDeactivatedStaffDto()
    // {
    //     // Arrange
    //     var deactivatedStaff = new Staff(
    //         _validStaff.LicenseNumber,
    //         _validStaff.Name,
    //         _validStaff.PhoneNumber,
    //         _validStaff.ContactEmail,
    //         _validStaff.StaffSpecialization,
    //         new Status("deactivated")
    //     );
    //
    //     _mockStaffRepository.Setup(repo => repo.GetByLicenseNumberAsync(_validStaff.LicenseNumber.Value))
    //         .ReturnsAsync(_validStaff);
    //     _mockStaffRepository.Setup(repo => repo.UpdateStatus(_validStaff.LicenseNumber.Value, It.IsAny<Staff>()))
    //         .ReturnsAsync(deactivatedStaff);
    //
    //     // Act
    //     var result = await _staffService.Deactivate(_validStaff.LicenseNumber.Value);
    //
    //     // Assert
    //     Assert.NotNull(result);
    //     Assert.Equal("deactivated", result.Status);
    //     _mockStaffRepository.Verify(repo => repo.UpdateStatus(_validStaff.LicenseNumber.Value, It.IsAny<Staff>()), Times.Once);
    // }
    //
    // [Fact]
    // public async Task Deactivate_WithNonExistingStaff_ReturnsNull()
    // {
    //     // Arrange
    //     _mockStaffRepository.Setup(repo => repo.GetByLicenseNumberAsync("nonexistent"))
    //         .ReturnsAsync((Staff)null);
    //
    //     // Act
    //     var result = await _staffService.Deactivate("nonexistent");
    //
    //     // Assert
    //     Assert.Null(result);
    // }
}
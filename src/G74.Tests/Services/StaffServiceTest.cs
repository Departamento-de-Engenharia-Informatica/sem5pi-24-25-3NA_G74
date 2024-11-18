using System.Collections.Generic;
using System.Threading.Tasks;
using G74.Domain.Aggregates.Staff;
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.DTO;
using G74.Services;
using JetBrains.Annotations;
using Moq;
using Xunit;
using System.Linq;

namespace G74.Tests.Services;

[TestSubject(typeof(StaffService))]
public class StaffServiceTest
{
    private readonly Mock<IUnitOfWork> _mockUnitOfWork;
    private readonly Mock<IStaffRepository> _mockStaffRepository;
    private readonly StaffService _staffService;
    private readonly Staff _sampleStaff;
    private readonly StaffDto _sampleStaffDto;

    public StaffServiceTest()
    {
        _mockUnitOfWork = new Mock<IUnitOfWork>();
        _mockStaffRepository = new Mock<IStaffRepository>();
        _staffService = new StaffService(_mockUnitOfWork.Object, _mockStaffRepository.Object);

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

        _sampleStaffDto = new StaffDto
        {
            LicenceNumber = 123456,
            Name = "John Doe",
            PhoneNumber = "912345678",
            ContactEmail = "john.doe@example.com",
            StaffSpecialization = "Cardiology",
            Status = "Active",
            Availability = "600-700"
        };
    }

    [Fact]
    public async Task GetAll_ReturnsAllStaff()
    {
        // Arrange
        var staffList = new List<Staff> { _sampleStaff };
        _mockStaffRepository.Setup(repo => repo.GetStaffAsync())
            .ReturnsAsync(staffList);

        // Act
        var result = await _staffService.GetAll();

        // Assert
        var staffDtoList = result.ToList();
        Assert.Single(staffDtoList);
        Assert.Equal(_sampleStaff.LicenceNumber.Value, staffDtoList[0].LicenceNumber);
        Assert.Equal(_sampleStaff.Name.Value, staffDtoList[0].Name);
        Assert.Equal(_sampleStaff.PhoneNumber.Value, staffDtoList[0].PhoneNumber);
        Assert.Equal(_sampleStaff.ContactEmail.email, staffDtoList[0].ContactEmail);
        Assert.Equal(_sampleStaff.StaffSpecialization.Value, staffDtoList[0].StaffSpecialization);
        Assert.Equal(_sampleStaff.Status.Value, staffDtoList[0].Status);
        Assert.Equal(_sampleStaff.Availability, staffDtoList[0].Availability);
    }

    [Fact]
    public async Task GetByLicenceNumber_ExistingStaff_ReturnsStaffDto()
    {
        // Arrange
        _mockStaffRepository.Setup(repo => repo.GetByLicenceNumber(It.Is<LicenceNumber>(ln => ln.Value == 123456)))
            .ReturnsAsync(_sampleStaff);

        // Act
        var result = await _staffService.GetByLicenceNumber(123456);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(_sampleStaff.LicenceNumber.Value, result.LicenceNumber);
        Assert.Equal(_sampleStaff.Name.Value, result.Name);
    }

    [Fact]
    public async Task GetByLicenceNumber_NonExistingStaff_ReturnsNull()
    {
        // Arrange
        _mockStaffRepository.Setup(repo => repo.GetByLicenceNumber(It.IsAny<LicenceNumber>()))
            .ReturnsAsync((Staff)null);

        // Act
        var result = await _staffService.GetByLicenceNumber(999999);

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task Add_ValidStaff_ReturnsStaffDto()
    {
        // Arrange
        // GetByLicenceNumber mock setup needed because it's used in the method in the service
        _mockStaffRepository.Setup(repo => repo.GetByLicenceNumber(It.IsAny<LicenceNumber>()))
            .ReturnsAsync((Staff)null);
        _mockStaffRepository.Setup(repo => repo.Add(It.IsAny<Staff>()))
            .ReturnsAsync(_sampleStaff);

        // Act
        var result = await _staffService.Add(_sampleStaffDto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(_sampleStaffDto.LicenceNumber, result.LicenceNumber);
        Assert.Equal(_sampleStaffDto.Name, result.Name);
        
        // // Without Unit of Work
        // staffRepository.Save(staff);        // First database operation
        // userRepository.Save(user);          // Second database operation
        // // If second operation fails, first one is already committed!
        //
        // // With Unit of Work
        // staffRepository.Add(staff);         // Just marks for insertion
        // userRepository.Add(user);           // Just marks for insertion
        // unitOfWork.CommitAsync();          // All changes are committed in one transaction
        // // If anything fails, nothing is committed
        
        // verify that CommitAsync() was called exactly once
        // This ensures that the service is properly committing the transaction
        _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
    }

    [Fact]
    public async Task Add_DuplicateLicenceNumber_ThrowsBusinessRuleValidationException()
    {
        // Arrange
        _mockStaffRepository.Setup(repo => repo.GetByLicenceNumber(It.IsAny<LicenceNumber>()))
            .ReturnsAsync(_sampleStaff);

        // Act & Assert
        await Assert.ThrowsAsync<BusinessRuleValidationException>(
            () => _staffService.Add(_sampleStaffDto)
        );
        _mockUnitOfWork.Verify(uow => uow.CommitAsync(), Times.Never);
    }

    [Fact]
    public async Task Update_ExistingStaff_ReturnsUpdatedStaffDto()
    {
        // Arrange
        var updatedStaff = Staff.Create(
            123456,
            "John Updated",
            "912345678",
            "john.updated@example.com",
            "Neurology",
            "Active",
            "700-800"
        );

        _mockStaffRepository.Setup(repo => repo.Update(It.IsAny<LicenceNumber>(), It.IsAny<Staff>()))
            .ReturnsAsync(updatedStaff);

        var updatedStaffDto = new StaffDto
        {
            LicenceNumber = 123456,
            Name = "John Updated",
            PhoneNumber = "912345678",
            ContactEmail = "john.updated@example.com",
            StaffSpecialization = "Neurology",
            Status = "Active",
            Availability = "700-800"
        };

        // Act
        var result = await _staffService.Update(123456, updatedStaffDto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(updatedStaffDto.Name, result.Name);
        Assert.Equal(updatedStaffDto.StaffSpecialization, result.StaffSpecialization);
    }

    [Fact]
    public async Task Update_NonExistingStaff_ThrowsBusinessRuleValidationException()
    {
        // Arrange
        _mockStaffRepository.Setup(repo => repo.Update(It.IsAny<LicenceNumber>(), It.IsAny<Staff>()))
            .ReturnsAsync((Staff)null);

        // Act & Assert
        await Assert.ThrowsAsync<BusinessRuleValidationException>(
            () => _staffService.Update(999999, _sampleStaffDto)
        );
    }

    [Fact]
    public async Task Deactivate_ExistingStaff_ReturnsDeactivatedStaffDto()
    {
        // Arrange
        var deactivatedStaff = Staff.Create(
            123456,
            "John Doe",
            "912345678",
            "john.doe@example.com",
            "Cardiology",
            "Inactive",
            "600-700"
        );

        _mockStaffRepository.Setup(repo => repo.GetByLicenceNumber(It.IsAny<LicenceNumber>()))
            .ReturnsAsync(_sampleStaff);
        _mockStaffRepository.Setup(repo => repo.Update(It.IsAny<LicenceNumber>(), It.IsAny<Staff>()))
            .ReturnsAsync(deactivatedStaff);

        // Act
        var result = await _staffService.Deactivate(123456);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Inactive", result.Status);
    }

    [Fact]
    public async Task Deactivate_NonExistingStaff_ReturnsNull()
    {
        // Arrange
        _mockStaffRepository.Setup(repo => repo.GetByLicenceNumber(It.IsAny<LicenceNumber>()))
            .ReturnsAsync((Staff)null);

        // Act
        var result = await _staffService.Deactivate(999999);

        // Assert
        Assert.Null(result);
    }
}
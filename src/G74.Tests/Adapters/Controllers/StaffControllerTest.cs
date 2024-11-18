using System.Collections.Generic;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.Domain.Shared;
using G74.DTO;
using G74.Services;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

[TestSubject(typeof(StaffController))]
public class StaffControllerTest
{

    private readonly Mock<IStaffService> _mockStaffService;
    private readonly StaffController _controller;

    public StaffControllerTest()
    {
        _mockStaffService = new Mock<IStaffService>();
        _controller = new StaffController(_mockStaffService.Object);
    }

    [Fact]
    public async Task GetStaff_ReturnsOkResult_WithListOfStaff()
    {
        // Arrange
        var expectedStaff = new List<StaffDto>
        {
            new StaffDto { 
                LicenceNumber = 123456,
                Name = "John Doe",
                PhoneNumber = "912345678",
                ContactEmail = "john.doe@example.com",
                StaffSpecialization = "Cardiology",
                Status = "Active",
                Availability = "600-700"
            },
            new StaffDto { 
                LicenceNumber = 810483,
                Name = "Jane Smith",
                PhoneNumber = "912345678",
                ContactEmail = "jane.smith@example.com",
                StaffSpecialization = "Dermatology",
                Status = "Active",
                Availability = "1000-1300"
            },
        };
        _mockStaffService.Setup(service => service.GetAll())
            .ReturnsAsync(expectedStaff);

        // Act
        var result = await _controller.GetStaff();

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result.Result);
        var returnValue = Assert.IsType<List<StaffDto>>(okResult.Value);
        Assert.Equal(expectedStaff, returnValue);
    }

    [Fact]
    public async Task GetStaffByLicenceNumber_ExistingStaff_ReturnsOkResult()
    {
        // Arrange
        long licenceNumber = 123456;
        var expectedStaff = new StaffDto
        {
            LicenceNumber = licenceNumber,
            Name = "John Doe",
            PhoneNumber = "912345678",
            ContactEmail = "john.doe@example.com",
            StaffSpecialization = "Cardiology",
            Status = "Active",
            Availability = "600-700"
        };
        _mockStaffService.Setup(service => service.GetByLicenceNumber(licenceNumber))
            .ReturnsAsync(expectedStaff);

        // Act
        var result = await _controller.GetStaffByLicenceNumber(licenceNumber);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result.Result);
        var returnValue = Assert.IsType<StaffDto>(okResult.Value);
        Assert.Equal(expectedStaff, returnValue);
    }

    [Fact]
    public async Task GetStaffByLicenceNumber_NonExistingStaff_ReturnsNotFound()
    {
        // Arrange
        long licenceNumber = 999999;
        _mockStaffService.Setup(service => service.GetByLicenceNumber(licenceNumber))
            .ReturnsAsync((StaffDto)null);

        // Act
        var result = await _controller.GetStaffByLicenceNumber(licenceNumber);

        // Assert
        Assert.IsType<NotFoundResult>(result.Result);
    }

    [Fact]
    public async Task RegisterStaff_ValidStaff_ReturnsCreatedAtAction()
    {
        // Arrange
        var staffDto = new StaffDto
        {
            LicenceNumber = 123456,
            Name = "John Doe",
            PhoneNumber = "912345678",
            ContactEmail = "john.doe@example.com",
            StaffSpecialization = "Cardiology",
            Status = "Active",
            Availability = "600-700"
        };
        _mockStaffService.Setup(service => service.Add(staffDto))
            .ReturnsAsync(staffDto);

        // Act
        var result = await _controller.RegisterStaff(staffDto);

        // Assert
        var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
        Assert.Equal(nameof(StaffController.GetStaffByLicenceNumber), createdAtActionResult.ActionName);
        Assert.Equal(staffDto.LicenceNumber, createdAtActionResult.RouteValues["licenceNumber"]);
        Assert.Equal(staffDto, createdAtActionResult.Value);
    }

    [Fact]
    public async Task RegisterStaff_InvalidStaff_ReturnsBadRequest()
    {
        // Arrange
        var staffDto = new StaffDto
        {
            LicenceNumber = 123456,
            Name = "John Doe",
            PhoneNumber = "912345678",
            ContactEmail = "john.doe@example.com",
            StaffSpecialization = "Cardiology",
            Status = "Active",
            Availability = "600-700"
        };
        _mockStaffService.Setup(service => service.Add(staffDto))
            .ThrowsAsync(new BusinessRuleValidationException("Invalid staff data"));

        // Act
        var result = await _controller.RegisterStaff(staffDto);

        // Assert
        var badRequestResult = Assert.IsType<BadRequestObjectResult>(result.Result);
        Assert.Contains("Invalid staff data", badRequestResult.Value.ToString());
    }
}
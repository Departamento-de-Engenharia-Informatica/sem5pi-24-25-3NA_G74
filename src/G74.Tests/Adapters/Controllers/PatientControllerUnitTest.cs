using System.Collections.Generic;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.Services;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

public class PatientControllerUnitTest
{
    private readonly Mock<IPatientAppService> _patientAppServiceMock;
    private readonly PatientController _controller;

    public PatientControllerUnitTest()
    {
        _patientAppServiceMock = new Mock<IPatientAppService>();
        _controller = new PatientController(_patientAppServiceMock.Object);
    }

    [Fact]
    public async Task RegisterPatient_ReturnsCreatedResult_WhenPatientIsRegistered()
    {
        var patientDto = new PatientDTO { /* Set test properties */ };
        _patientAppServiceMock.Setup(s => s.RegisterPatient(It.IsAny<PatientDTO>())).ReturnsAsync(patientDto);

        var result = await _controller.RegisterPatient(patientDto);

        Assert.IsType<CreatedAtActionResult>(result.Result);
    }

    [Fact]
    public async Task UpdatePatientLimited_ReturnsOkResult_WhenPatientIsUpdated()
    {
        var patientDto = new PatientDTO { /* Set test properties */ };
        _patientAppServiceMock.Setup(s => s.UpdatePatientLimited(It.IsAny<string>(), It.IsAny<PatientDTO>())).ReturnsAsync(patientDto);

        var result = await _controller.UpdatePatientLimited("MRN123", patientDto);

        Assert.IsType<OkObjectResult>(result.Result);
    }

    [Fact]
    public async Task DeletePatient_ReturnsOkResult_WhenPatientIsMarkedForDeletion()
    {
        _patientAppServiceMock.Setup(s => s.MarkPatientToBeDeleted(It.IsAny<string>())).Returns(Task.CompletedTask);

        var result = await _controller.DeletePatient("MRN123");

        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ListPatientsByFilter_ReturnsOkResult_WhenPatientsAreFound()
    {
        var patients = new List<PatientDTO> { new PatientDTO { /* Set test properties */ } };
        _patientAppServiceMock.Setup(s => s.SearchPatientsByFilters(It.IsAny<PatientDTO>())).ReturnsAsync(patients);

        var result = await _controller.ListPatientsByFilter(new PatientDTO());

        Assert.IsType<OkObjectResult>(result);
    }
}
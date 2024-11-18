
using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.DataModel;
using G74.Domain;
using G74.Domain.Aggregates.OperationType;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Infrastructure;
using G74.Services;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Http.HttpResults;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Moq;
using Newtonsoft.Json;
using Xunit;

[TestSubject(typeof(OperationRequestController))]
public class OperationRequestControllerTest: IClassFixture<WebApplicationFactory<G74.Startup>>
{

    private readonly HttpClient _client;
    private readonly Mock<IStaffRepository> _mockStaffRepository;
    private readonly Mock<StaffService> _staffService;
    private readonly OperationRequest _operationRequest;
    private readonly Mock<AuthController> _mockAuthController;
    private readonly Mock<BackofficeAppDbContext> _context;

    public OperationRequestControllerTest(WebApplicationFactory<G74.Startup> factory){
        _client = factory.CreateClient();
        _mockStaffRepository = new Mock<IStaffRepository>();
        Mock<IStaffRepository> _mockIStaffRepository = new Mock<IStaffRepository>();
        Mock<IUnitOfWork> _mockIUnitOfWork = new Mock<IUnitOfWork>();
        _staffService = new Mock<StaffService>(_mockIUnitOfWork, _mockIStaffRepository.Object);
        _operationRequest = new OperationRequest
        (
            new MedicalRecordNumber("202411000002"),
            new LicenceNumber(122005),
            1,
            new DeadlineDate(new DateTime()),
            new Priority("ElectiveSurgery")
        );
        _context = new Mock<BackofficeAppDbContext>();
        _mockAuthController = new Mock<AuthController>(_context);
        
       
    }

    // before all do login as doctor  
    
    internal async void DoLoginAsDoctor(){
        var mockAuthController = new Mock<AuthController>();
       
    }


    // testes unitários
    [Fact]
    public async Task RegisterOperationRequestTestSuccessfull(){
        var validOperationRequestDTO = new CreateOperationRequestDTO(
            "20241000002",
            12,
            2,
            DateTime.Now, 
            "ElectiveSurgery"
        );
        
        var operationService = new Mock<IAppServiceOperationRequest>();
        var operationRequestController = new OperationRequestController(operationService.Object);
        
        var response = await operationRequestController.RegisterOperationRequest(validOperationRequestDTO);
        
        var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(response.Result);
        Assert.NotNull(createdAtActionResult);
        Assert.Equal(201,createdAtActionResult.StatusCode);
    }

    [Fact]
    public async Task UpdateOperationRequestTestSuccessfull(){
        var validOperationRequestDTO = new CreateOperationRequestDTO(
            "202411000002",
            122005,
            1,
            DateTime.Now, 
            "ElectiveSurgery"
        );
        var operationService = new Mock<IAppServiceOperationRequest>();
        var operationRequestController = new OperationRequestController(operationService.Object);
        var response = await operationRequestController.UpdateOperationRequest("44fae03a-7df2-4434-b1d9-98e0e0872356", validOperationRequestDTO);
        
        var okResult = Assert.IsType<OkObjectResult>(response.Result);
        Assert.NotNull(okResult);
        Assert.Equal(200,okResult.StatusCode);
    }

    [Fact]
    public async Task DeleteOperationRequestTestSuccessfull(){
        var operationService = new Mock<IAppServiceOperationRequest>();
        var operationRequestController = new OperationRequestController(operationService.Object);
        var response = await operationRequestController.DeleteOperationRequest("44fae03a-7df2-4434-b1d9-98e0e0872356");

        var okResult = Assert.IsType<NoContentResult>(response.Result);
        Assert.NotNull(okResult);
        Assert.Equal(204,okResult.StatusCode);
    }

    [Fact]
    public async Task ReadAllOperationRequestSuccessfull(){
        var operationService = new Mock<IAppServiceOperationRequest>();
        var operationRequestController = new OperationRequestController(operationService.Object);
        var response = await operationRequestController.GetAllOperationRequest();

        var okResult = Assert.IsType<OkObjectResult>(response.Result);
        Assert.NotNull(okResult);
        Assert.Equal(200,okResult.StatusCode);
    }

    // testes integração
    [Fact]
    public async Task GetAllOperationRequestControllerTest(){
        var _mockRepositoryPatient = new Mock<IPatientAppService>();
        var _mockOperationRequestRepository = new Mock<IOperationRequestRepository>();
        var _mockStaffRepo = new Mock<IStaffRepository>();
        var _mockIUnitOfWork = new Mock<IUnitOfWork>();
        var _mockStaffService = new Mock<StaffService>(_mockIUnitOfWork.Object, _mockStaffRepo.Object);

        var operationService = new AppServiceOperationRequest(_mockOperationRequestRepository.Object, _mockRepositoryPatient.Object, _mockStaffService.Object);
        var operationRequestController = new OperationRequestController(operationService);

        var response = await operationRequestController.GetAllOperationRequest();

        var okResult = Assert.IsType<OkObjectResult>(response.Result);
        Assert.NotNull(okResult);
        Assert.Equal(200,okResult.StatusCode);
    }

    [Fact]
    public async Task CreateOperationRequestControllerTest(){
        var _mockRepositoryPatient = new Mock<IPatientAppService>();
        var _mockOperationRequestRepository = new Mock<IOperationRequestRepository>();
        var _mockStaffRepo = new Mock<IStaffRepository>();
        var _mockIUnitOfWork = new Mock<IUnitOfWork>();
        var _mockStaffService = new Mock<StaffService>(_mockIUnitOfWork.Object, _mockStaffRepo.Object);

        var operationService = new AppServiceOperationRequest(_mockOperationRequestRepository.Object, _mockRepositoryPatient.Object, _mockStaffService.Object);
        var operationRequestController = new OperationRequestController(operationService);
    
        var response = await operationRequestController.RegisterOperationRequest(new CreateOperationRequestDTO("202411000002", 102042, 1, new DateTime(), "UrgentSurgery"));
        var createdResult = Assert.IsType<CreatedAtActionResult>(response.Result);
        Assert.NotNull(createdResult);
        Assert.Equal(201, createdResult.StatusCode);

    }

// testes E2E
    [Fact]
    public async Task RegisterOperationRequestController(){
        
        //Arrange
        var createDataOperation = new CreateOperationRequestDTO(
            "202411000002",
            122005,
            1,
            new DateTime(),
            "ElectiveSurgery"
        );
        var operationJSON = new StringContent(JsonConvert.SerializeObject(createDataOperation),Encoding.UTF8, "application/json");
        
        //Act
         var response = await _client.PostAsync("/api/OperationRequest", operationJSON);

        //Assert
        response.EnsureSuccessStatusCode();
        Assert.Equal(HttpStatusCode.Created, response.StatusCode);
        var responseString = await response.Content.ReadAsStringAsync();
        Assert.NotNull(responseString);
    }

    [Fact]
    public async Task UpdateOperationRequestController(){
        //Arrange
        var createDataOperation = new CreateOperationRequestDTO(
            "202411000002",
            122005,
            1,
            new DateTime(),
            "ElectiveSurgery"
        );
        var operationJSON = new StringContent(JsonConvert.SerializeObject(createDataOperation),Encoding.UTF8, "application/json");
        
        //Act
         var response = await _client.PutAsync("/api/OperationRequest/25f6d228-4f72-4c24-80f2-dbc89354b40d", operationJSON);

        //Assert
        response.EnsureSuccessStatusCode();
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var responseString = await response.Content.ReadAsStringAsync();
        Assert.NotNull(responseString);
    }

    [Fact]
    public async Task RemoveOperationRequestController(){
        //Arrange
        var createDataOperation = new CreateOperationRequestDTO(
            "202411000002",
            122005,
            1,
            new DateTime(),
            "ElectiveSurgery"
        );
        var operationJSON = new StringContent(JsonConvert.SerializeObject(createDataOperation),Encoding.UTF8, "application/json");
        
        //Act
         var response = await _client.DeleteAsync("/api/OperationRequest/25f6d228-4f72-4c24-80f2-dbc89354b40d");

        //Assert
        response.EnsureSuccessStatusCode();
        Assert.Equal(HttpStatusCode.NoContent, response.StatusCode);
        var responseString = await response.Content.ReadAsStringAsync();
        Assert.NotNull(responseString);
    }

    [Fact]
    public async Task GetAllOperationRequestController(){
        //Arrange
        var createDataOperation = new CreateOperationRequestDTO(
            "202411000002",
            122005,
            1,
            new DateTime(),
            "ElectiveSurgery"
        );
        var operationJSON = new StringContent(JsonConvert.SerializeObject(createDataOperation),Encoding.UTF8, "application/json");
        
        //Act
         var response = await _client.GetAsync("/api/OperationRequest");

        //Assert
        response.EnsureSuccessStatusCode();
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var responseString = await response.Content.ReadAsStringAsync();
        Assert.NotNull(responseString);
    }



}
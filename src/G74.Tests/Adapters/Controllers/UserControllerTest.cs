using System;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.Adapters.Repositories;
using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Mappers;
using G74.Services;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Testing;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

[TestSubject(typeof(UserController))]
public class UserControllerTest : IClassFixture<WebApplicationFactory<G74.Startup>>
{
    
    private readonly HttpClient _client;

    public UserControllerTest(WebApplicationFactory<G74.Startup> factory)
    {
        _client = factory.CreateClient();
    }
    
    [Fact]
    public async Task RegisterNewUser_ValidJson_Success()
    {
        var mockRepoUser = new Mock<IRepoUser>();
        
        mockRepoUser.Setup(repo => repo.Save(It.IsAny<User>()))
            .ReturnsAsync((User user) => user); 
        
        var userMapper = new UserMapper();
        var userToDto = new UserToDTO();
        var userAppService = new UserAppService(userMapper, mockRepoUser.Object, userToDto);
        
        var userController = new UserController(userAppService, userToDto);
        
        JsonUserDTO newUser = new JsonUserDTO
        {
            Email = "afonso@gmail.com",
            Role = "Admin",
            Username = "afonso"
        };
        
        var result = await userController.RegisterNewUser(newUser);
        
        var createdResult = Assert.IsType<CreatedAtActionResult>(result.Result);
        
        Assert.NotNull(createdResult.Value);
    }
    
    [Fact]
    public async Task RegisterNewUser_InvalidJson_Failure()
    {
        var mockRepoUser = new Mock<IRepoUser>();
        
        mockRepoUser.Setup(repo => repo.Save(It.IsAny<User>()))
            .ReturnsAsync((User user) => user); 
        
        var userMapper = new UserMapper();
        var userToDto = new UserToDTO();
        var userAppService = new UserAppService(userMapper, mockRepoUser.Object, userToDto);
        
        var userController = new UserController(userAppService, userToDto);
        
        JsonUserDTO newUser = new JsonUserDTO
        {
            
        };
        
        var result = await userController.RegisterNewUser(newUser);
        
        var createdResult = Assert.IsType<BadRequestObjectResult>(result.Result);
        
        Assert.NotNull(createdResult);
    }

    [Fact]
    public async Task RegisterNewUser_ValidJson_ExistingUserFailure()
    {
        var mockRepoUser = new Mock<IRepoUser>();
        
        mockRepoUser.Setup(repo => repo.Save(It.IsAny<User>()))
            .ReturnsAsync((User user) => user); 
        
        mockRepoUser.Setup(repo => repo.UserExists(It.Is<Email>(email => email.email == "afonso@gmail.com")))
            .ReturnsAsync(true);

        var userMapper = new UserMapper();
        var userToDto = new UserToDTO();
        var userAppService = new UserAppService(userMapper, mockRepoUser.Object, userToDto);
        
        var userController = new UserController(userAppService, userToDto);
        
        JsonUserDTO newUser = new JsonUserDTO
        {
            Email = "afonso@gmail.com",
            Role = "Admin",
            Username = "afonso"
        };
        
        var result = await userController.RegisterNewUser(newUser);
        
        var createdResult = Assert.IsType<ConflictObjectResult>(result.Result);
        
        Assert.NotNull(createdResult.Value);
    }
    
}
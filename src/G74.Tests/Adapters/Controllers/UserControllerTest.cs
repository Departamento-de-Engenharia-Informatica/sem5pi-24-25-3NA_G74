using System;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
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
    /*
    private readonly HttpClient _client;

    public UserControllerTest(WebApplicationFactory<G74.Startup> factory)
    {
        _client = factory.CreateClient();
    }
    
    [Fact]
    public async Task RegisterNewUser_ShouldReturnCreatedStatus_WhenUserIsNew()
    {
        // Arrange
        var newUser = new
        {
            Username = "Afonso",
            Role.Admin,
            Email = "afonso@gmail.com"
        };
    
        var jsonContent = new StringContent(JsonSerializer.Serialize(newUser), Encoding.UTF8, "application/json");

        // Act
        var response = await _client.PostAsync("/api/user/register", jsonContent);

        // Assert
        Assert.Equal(HttpStatusCode.Created, response.StatusCode);
        var responseBody = await response.Content.ReadAsStringAsync();
        Assert.Contains("afonso@gmail.com", responseBody);
    }

    [Fact]
    public async Task GetUserByEmail_ShouldReturnUser_WhenUserExists()
    {
        // Arrange
        var email = "afonso@gmail.com";

        // Act
        var response = await _client.GetAsync($"/api/user/by-email/{email}");

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var responseBody = await response.Content.ReadAsStringAsync();
        Assert.Contains("afonso@gmail.com", responseBody);
    }

    [Fact]
    public async Task GetUserByEmail_ShouldReturnNotFound_WhenUserDoesNotExist()
    {
        // Arrange
        var email = "afonso@gmail.com";

        // Act
        var response = await _client.GetAsync($"/api/user/by-email/{email}");

        // Assert
        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
    }

    */
}
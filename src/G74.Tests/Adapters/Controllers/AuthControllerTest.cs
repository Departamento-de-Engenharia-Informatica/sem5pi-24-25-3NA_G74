using System;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Security.Claims;
using System.Threading.Tasks;
using G74.Adapters.Controllers;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using Google.Apis.Auth;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.EntityFrameworkCore;
using Moq;
using Xunit;

namespace G74.Tests.Adapters.Controllers;

[TestSubject(typeof(AuthController))]
public class AuthControllerTest : IClassFixture<WebApplicationFactory<Startup>>
{

    private readonly HttpClient _client;
    private readonly WebApplicationFactory<Startup> _factory;

    public AuthControllerTest(WebApplicationFactory<Startup> factory)
    {
        _factory = factory;
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task GoogleLogin_InvalidToken_ReturnsUnauthorized()
    {
        var googleLoginRequest = new 
        {
            Token = "invalid-google-token-simulated"
        };
        
        var response = await _client.PostAsJsonAsync("/api/auth/google-login", googleLoginRequest);
        
        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }
    

    
}
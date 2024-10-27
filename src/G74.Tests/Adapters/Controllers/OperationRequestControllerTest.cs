
using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Threading.Tasks;
using G74.Domain.Value_Objects.Staff;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Mvc.Testing;
using Xunit;

[TestSubject(typeof(OperationRequestController))]
public class OperationRequestControllerTest: IClassFixture<WebApplicationFactory<G74.Startup>>
{

    private readonly HttpClient _client;
    public OperationRequestControllerTest(WebApplicationFactory<G74.Startup> factory){
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task RegisterOperationRequestTestSuccessfull(){
        var list = new List<string>();
        
    }

}
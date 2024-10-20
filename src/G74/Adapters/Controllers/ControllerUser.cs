using System.Text.Json;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;


[ApiController]
[Route("api/[controller]")]
public class ControllerUser : ControllerBase
{
    private readonly AppServiceUser _appServiceUser;
    private readonly JsonToDTO _jsonToDto;

    public ControllerUser(AppServiceUser appServiceUser, JsonToDTO jsonToDto)
    {
        _appServiceUser = appServiceUser;
        _jsonToDto = jsonToDto;
    }

    public static bool ValidateJson(string json)
    {
        try
        {
            var user = JsonSerializer.Deserialize<VoUser>(json);

            
            if (user == null)
            {
                return false;
            }

            
            return !Username.VerifyUsername(user.Username.ToString()) &&
                   !Email.IsValidEmail(user.Email.ToString()) &&
                   Enum.IsDefined(typeof(Role), user.Role);
        }
        catch (JsonException)
        {
            return false;
        }
    }
    
    [HttpPost("register")]
    public IActionResult RegisterNewUser([FromBody] string json)
    {
        if (!ValidateJson(json))
        {
            return BadRequest(new { message = "Invalid user data." });
        }

        var voUser = MapToDtoUser(json);
        var userSaved = _appServiceUser.Create(voUser);
        var userJson = MapToJson(userSaved);
        
        return Ok(new { message = "User successfully registered.", user = userJson });
    }

    private VoUser MapToDtoUser(string json)
    {
            return _jsonToDto.CreateVoUser(json);
    }

    private string MapToJson(User userSaved)
    {
        return _jsonToDto.CreateJson(userSaved);
    }
}
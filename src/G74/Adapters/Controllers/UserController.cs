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
public class UserController : ControllerBase
{
    private readonly AppServiceUser _appServiceUser;
    private readonly UserToDTO _userToDto;

    public UserController(AppServiceUser appServiceUser, UserToDTO userToDto)
    {
        _appServiceUser = appServiceUser;
        _userToDto = userToDto;
    }

  /*  public static bool ValidateJson(string json)
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
    */
  
    [HttpPost("register")]
    public async Task<ActionResult<UserDTO>> RegisterNewUser([FromBody]UserDTO uDto)
    {
        try
        {
            var userDto = await _appServiceUser.Create(uDto);
            return CreatedAtAction(nameof(GetUserByEmail), userDto);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
        
    }
    
    [HttpGet("by-email/{email}")]
    public async Task<ActionResult<UserDTO>> GetUserByEmail(string email)
    {
        try
        {
            var userDto = await _appServiceUser.GetUserByEmail(email);
            return Ok(userDto);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
    }

}
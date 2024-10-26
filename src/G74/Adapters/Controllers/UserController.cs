using System.Text.Json;
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;


[ApiController]
[Route("api/[controller]")]
public class UserController : ControllerBase
{
    private readonly UserAppService _userAppService;
    private readonly UserToDTO _userToDto;

    public UserController(UserAppService userAppService, UserToDTO userToDto)
    {
        _userAppService = userAppService;
        _userToDto = userToDto;
    }
  
    [Authorize(Roles = "Admin")]
    [HttpPost("register")]
    public async Task<ActionResult<UserDTO>> RegisterNewUser([FromBody]JsonUserDTO jsonUserDto)
    {
        try
        {
            UserDTO uDto = _userToDto.JsonToDTO(jsonUserDto);
            UserDTO userDto = await _userAppService.Create(uDto);
            if (userDto == null)
            {
                return new ConflictObjectResult(new { message = "User already exists with the given email." });
            }
            return CreatedAtAction(nameof(RegisterNewUser), userDto);
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
            var userDto = await _userAppService.GetUserByEmail(email);
            return Ok(userDto);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
    }

}
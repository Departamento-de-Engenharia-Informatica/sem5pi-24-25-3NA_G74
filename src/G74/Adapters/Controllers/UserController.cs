using System.Security.Claims;
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

    public UserController(UserAppService userAppService)
    {
        _userAppService = userAppService;
    }
  
    [Authorize(Roles = "Admin, Patient")]
    [HttpPost("register")]
    public async Task<ActionResult<UserDto>> RegisterNewUser([FromBody]UserDto receivedUserDto)
    {
        try
        {
            UserDto userDto = await _userAppService.Create(receivedUserDto);
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
    
    [HttpGet("by-email")]
    public async Task<ActionResult<UserDto>> GetLoggedUserByEmail()
    {
        try
        {
            var email = HttpContext.User.FindFirstValue(ClaimTypes.Email);
            if (string.IsNullOrEmpty(email))
            {
                return Unauthorized(new { success = false, message = "User is not authenticated." });
            }
            var userDto = await _userAppService.GetUserByEmail(email);
            return Ok(userDto);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
    }

    [Authorize(Roles = "Admin, Patient")]
    [HttpPut("update")]
    public async Task<ActionResult<UserDto>> UpdateUser([FromBody]UserDto receivedUserDto)
    {
        try
        {
            var currentUserDto = GetLoggedUserByEmail().Result.Value;
            if (currentUserDto == null)
            {
                return NotFound(new { message = "User not found." });
            }
            var userDto = await _userAppService.UpdateUser(receivedUserDto, currentUserDto);

            return Ok(new
            {
                message = $"User was updated successfully.", 
                userDto
            });
        }
        catch (Exception e)
        {
            return BadRequest((e.Message));
        }
    }
    
    
}
using System.Security.Claims;
using G74.DTO;

namespace G74.Services;

public interface ITokenService
{
    
    string GenerateJwtToken(UserDto userDto);

    ClaimsPrincipal ValidateToken(string token);
    
}
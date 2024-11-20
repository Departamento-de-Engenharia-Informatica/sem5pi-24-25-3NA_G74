using G74.DTO;

namespace G74.Services;

public interface IAuthService
{
    Task<(string? token, string errorMessage, UserDto? userDto)> Login(LoginDto loginDto);
    Task<(string? token, string errorMessage, UserDto? userDto)> IAMLogin(string idToken);
}
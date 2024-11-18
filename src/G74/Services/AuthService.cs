using G74.DTO;

namespace G74.Services;

public class AuthService : IAuthService
{


    private readonly ITokenService _tokenService;
    private readonly UserAppService _userAppService;

    public AuthService(ITokenService tokenService, UserAppService userAppService)
    {
        _tokenService = tokenService;
        _userAppService = userAppService;
    }
    
    public async Task<(string? token, string errorMessage, UserDto? userDto)> Login(LoginDto loginDto)
    {

        var userDtoFound = await _userAppService.GetUserByEmail(loginDto.Email);

        if (userDtoFound == null) return (null, "User not found", null);
        
        //Can add an encrypted password to the user attributes, so that logins also checks for the right password


        var token = _tokenService.GenerateJwtToken(userDtoFound);
        
        
        return (token,"Logged in successfully",userDtoFound);
    }
}
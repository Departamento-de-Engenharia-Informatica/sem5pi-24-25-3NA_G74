using G74.DTO;
using Google.Apis.Auth;

namespace G74.Services;

public class AuthService : IAuthService
{
    private readonly ITokenService _tokenService;
    private readonly UserAppService _userAppService;
    private readonly IProviderIAMService _providerIamService;

    public AuthService(ITokenService tokenService, UserAppService userAppService, IProviderIAMService iamService)
    {
        _tokenService = tokenService;
        _userAppService = userAppService;
        _providerIamService = iamService;
    }

    public async Task<(string? token, string errorMessage, UserDto? userDto)> Login(LoginDto loginDto)
    {

        //Can add an encrypted password to the user attributes, so that logins also checks for the right password
        try
        {
            var userDtoFound = await _userAppService.GetUserByEmail(loginDto.Email);

            var token = _tokenService.GenerateJwtToken(userDtoFound);
            
            return (token, "Logged in successfully", userDtoFound);
        }
        catch (ArgumentNullException e)
        {
            return (null, "User has not yet registered in the application", null);
        }
        
    }

    public async Task<(string? token, string errorMessage, UserDto? userDto)> IAMLogin(string idToken)
    {
        try
        {
            var email = _providerIamService.AuthenticateProviderTokenAsync(idToken);

            if (email.Result == null) return (null, "No email received from IAM provider payload", null);

            var userDto = _userAppService.GetUserByEmail(email.Result);
            
            var token = _tokenService.GenerateJwtToken(userDto.Result);

            return (token, "Logged in successfully", userDto.Result);
        }
        catch (InvalidJwtException e)
        {
            throw new InvalidOperationException(e.Message, e.InnerException);
        }
        catch (ArgumentNullException e)
        {
                return (null, "User has not yet registered in the application", null);
        }
    }
}
using Google.Apis.Auth;

namespace G74.Services;

public class GoogleIAMService : IProviderIAMService
{
    public async Task<string> AuthenticateProviderTokenAsync(string idToken)
    {
        var payload = await GoogleJsonWebSignature.ValidateAsync(idToken);
        
        return payload.Email;
    }
}
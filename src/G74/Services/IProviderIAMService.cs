namespace G74.Services;

public interface IProviderIAMService
{

    Task<string> AuthenticateProviderTokenAsync(string idToken);


}
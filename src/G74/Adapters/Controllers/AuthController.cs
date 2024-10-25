using G74.Domain.Aggregates.User;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Mvc;
using Google.Apis.Auth;
using Microsoft.EntityFrameworkCore;
using System.Security.Claims;
using G74.Domain.Value_Objects;
using G74.DTO;

namespace G74.Adapters.Controllers
{
    [ApiController]
    [Route("api/auth")]
    public class AuthController : ControllerBase
    {
        //private readonly AuthAppService _authAppService;
        private readonly BackofficeAppDbContext _context;

        public AuthController(BackofficeAppDbContext context)
        {
            _context = context;
            //_authAppService = authAppService;
        }

        [HttpPost("google-login")]
        public async Task<IActionResult> GoogleLogin([FromBody] GoogleLoginRequest request)
        {
            try
            {
                var payload = await GoogleJsonWebSignature.ValidateAsync(request.Token);
                Email loginEmail = new Email(payload.Email);
                var userDataModel = await _context.Users.SingleOrDefaultAsync(u => u.Email.Equals(loginEmail));
                
                if (userDataModel == null)
                {
                    return NotFound(new { success = false, message = "User does not exist." });
                }

                // Isto é temporario apenas para testar em postman. o codigo real é await GeneratedAuthToken(userDataModel)
                var authToken = GenerateAuthToken(userDataModel);
                Console.WriteLine($"Generate Token: {authToken}");

                return Ok(new { success = true, message = "Login successful!" });
            }
            catch (InvalidJwtException)
            {
                return Unauthorized(new { success = false, message = "Invalid token, unauthorized user detected." });
            }
        }

        private async Task GenerateAuthToken(UserDataModel userDataModel)
        {
            var claims = new List<Claim>
            {
                new Claim(ClaimTypes.NameIdentifier, userDataModel.Id.ToString()),
                new Claim(ClaimTypes.Email, userDataModel.Email.email),
                new Claim(ClaimTypes.Role, userDataModel.Role.ToString())
            };
            
            var claimsIdentity = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme);
            var authProperties = new AuthenticationProperties
            {
                IsPersistent = true
            };
            
            await HttpContext.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, new ClaimsPrincipal(claimsIdentity), authProperties);
        }

        [HttpPost("logout")]
        public async Task<IActionResult> Logout()
        {
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return Ok(new { success = true, message = "Logout done successfully." });
        }
    }
}

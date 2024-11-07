using System.IdentityModel.Tokens.Jwt;
using G74.Domain.Aggregates.User;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Mvc;
using Google.Apis.Auth;
using Microsoft.EntityFrameworkCore;
using System.Security.Claims;
using System.Text;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Infrastructure;
using Microsoft.AspNetCore.Authorization;
using Microsoft.IdentityModel.Tokens;

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
        
        [AllowAnonymous]
        [HttpPost("google-login")]
        public async Task<IActionResult> GoogleLogin([FromBody] GoogleLoginRequest request)
        {
            try
            {
                var payload = await GoogleJsonWebSignature.ValidateAsync(request.Token);
                Console.WriteLine("Token validado com sucesso. Informações do payload:");
                Console.WriteLine($"Email: {payload.Email}");
                Console.WriteLine($"Nome: {payload.Name}");
                Console.WriteLine($"ID: {payload.Subject}");
                Email loginEmail = new Email(payload.Email);
                var userDataModel = await _context.Users.SingleOrDefaultAsync(u => u.Email.Equals(loginEmail));
                
                if (userDataModel == null)
                {
                    return NotFound(new { success = false, message = "User does not exist." });
                }
                
                await GenerateAuthToken(userDataModel);
                

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
                new Claim(ClaimTypes.Email, userDataModel.Email),
                new Claim(ClaimTypes.Role, userDataModel.Role)
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
            foreach (var cookie in HttpContext.Request.Cookies.Keys)
            {
                HttpContext.Response.Cookies.Delete(cookie);
            }
            return Ok(new { success = true, message = "Logout done successfully." });
        }
    }
}

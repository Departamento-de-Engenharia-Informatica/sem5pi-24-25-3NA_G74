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
using G74.DTO;
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
                
                // NAO APAGAR ESTE COMENTARIO EM BAIXO.
                // O GenerateAuthToken vai gerar o token que permite criar sessoes para cada user.
                // Isto vai ser o principal executor de autorizaçoes e restriçao de funcionalidades.
                // Por enquanto vou usar JWT só para gerar um token de autorizaçao e usar como teste e demonstraçao no postman.
                await GenerateAuthToken(userDataModel);
                /*var authToken = GenerateJwtToken(userDataModel);
                Console.WriteLine($"Generated Token: {authToken}"); // Imprima o token na consola*/

                

                return Ok(new { success = true, message = "Login successful!" });
            }
            catch (InvalidJwtException)
            {
                return Unauthorized(new { success = false, message = "Invalid token, unauthorized user detected." });
            }
        }

        // Método principal a ser usado em conjunto com o GUI para criar sessoes para cada user.
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
        
        
        /*private string GenerateJwtToken(UserDataModel userDataModel)
        {
            var claims = new List<Claim>
            {
                new Claim(ClaimTypes.NameIdentifier, userDataModel.Id.ToString()),
                new Claim(ClaimTypes.Email, userDataModel.Email.email),
                new Claim(ClaimTypes.Role, userDataModel.Role.ToString())
            };

            
            var secretKey = Convert.FromBase64String("c2VjcmV0S2V5MjAwQmFzZTY0U2VjdXJlQ3VzdG9tHANddai3ndaBFude"); 
            var key = new SymmetricSecurityKey(secretKey);
            var creds = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);

            var token = new JwtSecurityToken(
                issuer: "localhost", 
                audience: "localhost", 
                claims: claims,
                expires: DateTime.Now.AddMinutes(30), 
                signingCredentials: creds);

            return new JwtSecurityTokenHandler().WriteToken(token);
        }*/

        [HttpPost("logout")]
        public async Task<IActionResult> Logout()
        {
            await HttpContext.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme);
            return Ok(new { success = true, message = "Logout done successfully." });
        }
    }
}

using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class AuthController : ControllerBase
    {
        
        private readonly IAuthService _authService;
        

        public AuthController(IAuthService authService)
        {
            _authService = authService;
            
        }

        [HttpPost("login")]
        public async Task<IActionResult> Login([FromBody] LoginDto? loginDto)
        {
            if (loginDto == null) return BadRequest("Login data not valid");

            var (token, message, userDto) = await _authService.Login(loginDto);

            return token == null
                ? StatusCode(500, message)
                : Ok(new { Token = token, Message = message, User = userDto });
        }

        [HttpPost("iam-login")]
        public async Task<IActionResult> IAMLogin([FromBody] IAMLoginRequest request)
        {
            if (request.Token == null) return BadRequest("Google token not received");

            var (token, message, userDto) = await _authService.IAMLogin(request.Token);

            return token == null
                ? StatusCode(500, message)
                : Ok(new { Token = token, Message = message, User = userDto });
        }


        /*

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
                var userDataModel = await _context.Users.SingleOrDefaultAsync(u => u.Email.Equals(loginEmail.ToString()));

                if (userDataModel == null)
                {
                    return NotFound(new { success = false, message = "User does not exist." });
                }

                await GenerateAuthToken(userDataModel);


                return Ok(new { success = true, message = "Login successful!", role = userDataModel.Role });
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

        [HttpGet("check-session")]
        public IActionResult CheckSession()
        {
            if (User.Identity.IsAuthenticated)
            {
                return Ok(new { isAuthenticated = true });
            }
            return Ok(new { isAuthenticated = false });
        }
        */
    }
}
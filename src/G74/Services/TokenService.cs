using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using G74.DTO;
using Microsoft.IdentityModel.Tokens;

namespace G74.Services;

public class TokenService : ITokenService
{
    private readonly IConfiguration _configuration;
    private readonly IHttpContextAccessor _httpContextAccessor;

    public TokenService(IConfiguration configuration, IHttpContextAccessor httpContextAccessor)
    {
        _configuration = configuration;
        _httpContextAccessor = httpContextAccessor;
    }


    public string GenerateJwtToken(UserDto userDto)
    {
        var claims = new List<Claim>
        {
            new Claim(ClaimTypes.Name, userDto.Username),
            new Claim(ClaimTypes.Role, userDto.Role),
            new Claim(ClaimTypes.Email, userDto.Email),
        };


        return GenerateToken(claims,
            DateTime.Now.AddMinutes(int.Parse(_configuration["JwtSettings:TokenExpirationMinutes"] ??
                                              throw new InvalidOperationException(
                                                  "Token expiration period not set up"))));
    }

    public string GenerateToken(IEnumerable<Claim> claims, DateTime expiration)
    {
        // Read JWT settings from configuration
        var secretKey = _configuration["JwtSettings:SecretKey"] ??
                        throw new InvalidOperationException("token secret key not set up");

        var issuer = _configuration["JwtSettings:Issuer"];
        var audience = _configuration["JwtSettings:Audience"];


        var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
        var creds = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);

        var token = new JwtSecurityToken(
            issuer: issuer,
            audience: audience,
            claims: claims,
            expires: expiration, // Use the provided expiration
            signingCredentials: creds
        );

        return new JwtSecurityTokenHandler().WriteToken(token); // Return the token as a string
    }

    public ClaimsPrincipal ValidateToken(string token)
    {
        var tokenHandler = new JwtSecurityTokenHandler();
        var secretKey = _configuration["JwtSettings:SecretKey"];
        var key = Encoding.UTF8.GetBytes(secretKey);
        try
        {
            return tokenHandler.ValidateToken(token, new TokenValidationParameters
            {
                ValidateIssuerSigningKey = true,
                IssuerSigningKey = new SymmetricSecurityKey(key),
                ValidateIssuer = true,
                ValidateAudience = true,
                ValidIssuer = _configuration["JwtSettings:Issuer"],
                ValidAudience = _configuration["JwtSettings:Audience"],
                ClockSkew = TimeSpan.Zero
            }, out SecurityToken validatedToken);
        }
        catch (SecurityTokenExpiredException)
        {
            throw new SecurityTokenException("Token has expired.");
        }
        catch (Exception ex)
        {
            throw new SecurityTokenException("Invalid token.", ex);
        }
    }
}
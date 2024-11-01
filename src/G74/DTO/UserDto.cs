using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;

public class UserDto
{
    public string Username { get; set; }
    public string Email { get; set; }
    public string Role { get; set; }
    
    public UserDto(string username, string email, string role)
    {
        Username = username;
        Email = email;
        Role = role;
    }
}
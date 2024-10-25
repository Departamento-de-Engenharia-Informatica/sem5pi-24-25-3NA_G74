using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;

public class UserDTO
{
    public Username Username { get; set; }
    public Email Email { get; set; }
    public Role Role { get; set; }
    
    public UserDTO(string username, string email, string role)
    {
        Username = new Username(username);
        Email = new Email(email);
        if (Enum.TryParse(role, true, out Role parsedRole))
        {
            Role = parsedRole;
        }
        else
        {
            throw new ArgumentException("Invalid role value");
        }
    }
}
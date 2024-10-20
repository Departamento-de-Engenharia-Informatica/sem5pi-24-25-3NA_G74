using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Domain.Aggregates.User;

public class User
{
    private Username username { get; set; }
    private Role role { get; set; }
    private Email email { get; set; }

    public User(Username username, Role role, Email email)
    {
        this.username = username ?? throw new ArgumentNullException(nameof(username));
        this.role = role;
        this.email = email ?? throw new ArgumentNullException(nameof(email));
    }
    
    public string GetUsername(User user)
    {
        return user.username.ToString();
    }

    public string GetEmail(User user)
    {
        return user.email.ToString();
    }

    public string GetRole(User user)
    {
        return user.role.ToString();
    }
    
}
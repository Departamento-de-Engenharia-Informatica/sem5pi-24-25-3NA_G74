using G74.Domain.Aggregates.Patient;
using G74.Domain.Shared;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Domain.Aggregates.User;

public class User
{
    public Username username { get; set; }
    public Role role { get; set; }
    public Email email { get; set; }

    public User(Username username, Role role, Email email)
    {
        this.username = username ?? throw new ArgumentNullException(nameof(username));
        this.role = role;
        this.email = email ?? throw new ArgumentNullException(nameof(email));
    }
    
    public string GetUsername()
    {
        return username.ToString();
    }

    public string GetEmail()
    {
        return email.ToString();
    }

    public string GetRole()
    {
        return role.ToString();
    }
    
}
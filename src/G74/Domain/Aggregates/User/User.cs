using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.Aggregates.User;

public class User
{
    public Username username { get; set; }
    public Role role { get; set; }
    public Email email { get; set; }

    public User(Username username, Role role, Email email)
    {
        this.username = username ?? throw new ArgumentNullException(nameof(username));
        if (!Enum.IsDefined(typeof(Role), role))
        {
            throw new ArgumentException("Invalid role", nameof(role));
        }
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
    
    public void UpdateUsername(Username newUsername)
    {
        username = newUsername ?? throw new ArgumentNullException(nameof(newUsername), "Username cannot be null when updating");
    }
    
    public void UpdateEmail(Email newEmail)
    {
        email = newEmail ?? throw new ArgumentNullException(nameof(newEmail), "Name cannot be null when updating");
    }
    public void UpdateRole(string newRole)
    {
        if (string.IsNullOrWhiteSpace(newRole))
        {
            throw new ArgumentNullException(nameof(newRole), "Role cannot be null or empty when updating");
        }

        if (!Enum.TryParse(typeof(Role), newRole, true, out var parsedRole) || !Enum.IsDefined(typeof(Role), parsedRole))
        {
            throw new ArgumentException($"Invalid role: {newRole}", nameof(newRole));
        }

        role = (Role)parsedRole;
    }

    
}
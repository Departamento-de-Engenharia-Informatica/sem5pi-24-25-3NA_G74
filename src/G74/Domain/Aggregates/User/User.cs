using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.Aggregates.User;

public class User
{
    private int id { get; set; }
    private Username username { get; set; }
    private Role role { get; set; }
    private Email email { get; set; }

    public User(Username username, Role role, Email email)
    {
        this.username = username ?? throw new ArgumentNullException(nameof(username));
        this.role = role;
        this.email = email ?? throw new ArgumentNullException(nameof(email));
    }
}
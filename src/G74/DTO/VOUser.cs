using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;

public class VoUser
{
    public Username Username { get; }
    public Email Email { get; }
    public Role Role { get; }

    public VoUser(Username username, Email email, Role role)
    {
        Username = username ?? throw new ArgumentNullException(nameof(username));
        Email = email ?? throw new ArgumentNullException(nameof(email));
        Role = role;
    }
}

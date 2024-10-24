using System.ComponentModel.DataAnnotations;
using G74.Domain.Aggregates.User;
using G74.Domain.Shared;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;

public class UserDataModel : Entity<Guid>
{
    public Username Username { get; private set; }
    public Email Email { get; private set; }
    public Role Role { get; private set; }

    protected UserDataModel() : base(Guid.NewGuid())
    {
        
    }
    public UserDataModel(User user) : base(Guid.NewGuid())
    {
        Username = user.username;
        Email = user.email;
        Role = user.role;
    }
}

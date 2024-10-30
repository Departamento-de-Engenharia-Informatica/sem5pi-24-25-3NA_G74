using System.ComponentModel.DataAnnotations;
using G74.Domain.Aggregates.User;
using G74.Domain.Shared;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;

public class UserDataModel : Entity<Guid>
{
    public string Username { get; private set; }
    public string Email { get; private set; }
    public string Role { get; private set; }

    protected UserDataModel() : base(Guid.NewGuid())
    {
        
    }
    public UserDataModel(User user) : base(Guid.NewGuid())
    {
        Username = user.username.name;
        Email = user.email.email;
        Role = user.role.ToString();
    }
}

using G74.Domain.Aggregates.User;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;

namespace G74.DTO;

public class UserDataModel : Entity<Guid>
{
    public string Username { get; private set; }
    public string Email { get; private set; }
    public string Role { get; private set; }
    public bool MarkedForDeletion { get; set; }

    public DateTime? DateToBeDeleted { get; set; }

    protected UserDataModel() : base(Guid.NewGuid())
    {
        
    }
    public UserDataModel(User user) : base(Guid.NewGuid())
    {
        Username = user.username.name;
        Email = user.email.email;
        Role = user.role.ToString();
        MarkedForDeletion = false;
        DateToBeDeleted = null;
    }
    
    public void UpdateUsername(string newName)
    {
        Username = newName;
    }

    public void UpdateEmail(string newEmail)
    {
        Email = newEmail;
    }

    public void UpdateRole(string newRole)
    {
        Role = newRole;
    }
    
    public void MarkForDeletion(TimeSpan timeToDeletion)
    {
        if (timeToDeletion != null)
        {
            MarkedForDeletion = true;
            DateToBeDeleted = DateTime.UtcNow.Add(timeToDeletion);
        }
    }
}

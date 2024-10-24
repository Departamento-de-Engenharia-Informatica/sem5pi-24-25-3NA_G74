using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Mappers;

public class UserToDataMapper
{
    public UserDataModel MapToDataUser(User user)
    {
        if (user == null)
        {
            throw new ArgumentNullException(nameof(user), "User cannot be null.");
        }

        UserDataModel userDataModel = new UserDataModel
        (
            user.GetUsername(),
            user.GetEmail(),     
            user.GetRole()    
        );

        return userDataModel;
    }

    public User MapToUser(UserDataModel savedUser)
    {
        if (savedUser == null)
        {
            throw new ArgumentNullException(nameof(savedUser), "DataUser cannot be null.");
        }
        
        if (!Enum.TryParse<Role>(savedUser.Role, true, out Role role))
        {
            throw new ArgumentException($"Invalid role: {savedUser.Role}");
        }
        
        return new User
        (
            new Username(savedUser.Username),  
            role,     
            new Email(savedUser.Email)
        );
    }
}
using G74.Domain.Aggregates.User;
using G74.Domain.Shared;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Mappers;

public class UserToDataModelMapper
{
    public UserDataModel MapToDataModel(User user)
    {
        if (user == null)
        {
            throw new ArgumentNullException(nameof(user), "User cannot be null.");
        }

        UserDataModel userDataModel = new UserDataModel
        (
            user   
        );

        return userDataModel;
    }

    public User MapToUser(UserDataModel savedUser)
    {
        if (savedUser == null)
        {
            throw new ArgumentNullException(nameof(savedUser), "DataUser cannot be null.");
        }
        if (!Enum.TryParse<Role>(savedUser.Role, true, out var role))
        {
            throw new BusinessRuleValidationException("Invalid role.");
        }
        
        return new User
        (
            new Username(savedUser.Username),
            role,
            new Email(savedUser.Email)
        );
    }
    
    public bool UpdateDataModel(UserDataModel userDataModel, User user)
    {
        userDataModel.UpdateUsername(user.username.name);
        userDataModel.UpdateEmail(user.email.email);
        userDataModel.UpdateRole(user.role.ToString());
        return true;
    }
}
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
        
        
        return new User
        (
            savedUser.Username,  
            savedUser.Role,     
            savedUser.Email
        );
    }
}
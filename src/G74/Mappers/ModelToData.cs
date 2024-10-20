using G74.Domain.Aggregates.User;
using G74.DTO;

namespace G74.Mappers;

public class ModelToData
{
    public DataUser MapToDataUser(User user)
    {
        if (user == null)
        {
            throw new ArgumentNullException(nameof(user), "User cannot be null.");
        }

        DataUser dataUser = new DataUser
        (
            user.GetUsername(user),
            user.GetEmail(user),     
            user.GetRole(user)    
        );

        return dataUser;
    }
}
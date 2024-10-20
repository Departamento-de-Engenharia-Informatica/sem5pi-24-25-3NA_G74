using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Mappers;

public class UserMapper
{
    public static User Create(VoUser voUser)
    {
        return new User(voUser.Username, voUser.Role, voUser.Email);
    }
}
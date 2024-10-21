using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Mappers;

public class UserMapper
{
    public User Create(UserDTO userDto)
    {
        return new User(userDto.Username, userDto.Role, userDto.Email);
    }
}
using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using Microsoft.AspNetCore.Identity;

namespace G74.DTO;

using System;
using System.Text.Json;

public class UserToDTO
{
    static public UserDTO DomainToDTO(User user) {

        UserDTO userDto = new UserDTO(new Username(user.GetUsername()), new Email(user.GetEmail()), user.role);

        return userDto;
    }
}

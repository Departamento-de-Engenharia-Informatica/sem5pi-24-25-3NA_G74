using G74.Domain.Aggregates.User;
using G74.Domain.Shared;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using Microsoft.AspNetCore.Identity;

namespace G74.DTO;

using System;
using System.Text.Json;

public class UserToDtoMapper
{
    public UserDto UserToDto(User user) {

        if (user == null)
        {
            throw new ArgumentNullException(nameof(user), "User cannot be null.");
        }
        
        UserDto userDto = new UserDto(user.username.name, user.email.email, user.role.ToString());

        return userDto;
    }

    public User DtoToUser(UserDto userDto)
    {
        if (userDto == null)
        {
            throw new ArgumentNullException(nameof(userDto), "UserDto cannot be null.");
        }
        if (!Enum.TryParse<Role>(userDto.Role, true, out var role))
        {
            throw new BusinessRuleValidationException("Invalid role.");
        }
        User user = new User(new Username(userDto.Username), role, new Email(userDto.Email));
        return user;
    }

    public UserDto Create(string username, string email, string role)
    {
        UserDto userDto = new UserDto(username, email, role);
        return userDto;
    }
}

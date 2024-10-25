using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using Microsoft.AspNetCore.Identity;

namespace G74.DTO;

using System;
using System.Text.Json;

public class UserToDTO
{
    public UserDTO DomainToDTO(User user) {

        UserDTO userDto = new UserDTO(user.username.name, user.email.email, user.GetRole());

        return userDto;
    }

    public UserDTO JsonToDTO(JsonUserDTO jsonUserDto)
    {
        UserDTO userDto = new UserDTO(jsonUserDto.Username, jsonUserDto.Email, jsonUserDto.Role);
        return userDto;
    }
}

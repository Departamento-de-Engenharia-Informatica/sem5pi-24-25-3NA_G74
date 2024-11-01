using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Mappers;
using Microsoft.AspNetCore.Mvc;

namespace G74.Services;

public class UserAppService
{
    private readonly IRepoUser _repoUser;
    private readonly UserToDtoMapper _userToDtoMapper;

    public UserAppService(IRepoUser repoUser, UserToDtoMapper userToDtoMapper)
    {
        _userToDtoMapper = userToDtoMapper;
        _repoUser = repoUser;
    }
    
    public async Task<UserDto> Create(UserDto receivedUserDto)
    {
        bool userExists = await _repoUser.UserExists(receivedUserDto.Email);
        if(userExists) {
            return null;
        }
        User user = _userToDtoMapper.DtoToUser(receivedUserDto);
        User userSaved = await _repoUser.Save(user);
        UserDto userDto = _userToDtoMapper.UserToDto(userSaved);
        return userDto;
    }
    
    public async Task<UserDto> GetUserByEmail(string email)
    {
        var existingUser = await _repoUser.GetUserByEmail(email);

        return _userToDtoMapper.UserToDto(existingUser);
    }
    
    public async Task<UserDto> UpdateUser(UserDto receivedUserDto, UserDto currentUserDto)
    {
        if (currentUserDto == null)
        {
            throw new ArgumentNullException(nameof(currentUserDto), "Current user data cannot be null.");
        }

        string oldEmail = currentUserDto.Email;
        
        UserDto updatedUserDto = _userToDtoMapper.Create(
            username: receivedUserDto.Username ?? currentUserDto.Username,
            email: receivedUserDto.Email ?? currentUserDto.Email,
            role: receivedUserDto.Role ?? currentUserDto.Role
        );
        User updatedUser = _userToDtoMapper.DtoToUser(updatedUserDto);
        User savedUser = await _repoUser.UpdateUser(updatedUser, oldEmail);
        UserDto userDto = _userToDtoMapper.UserToDto(savedUser);

        return userDto;
    }
    
}
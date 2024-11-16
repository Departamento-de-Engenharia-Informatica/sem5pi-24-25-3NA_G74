using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Mappers;
using Microsoft.AspNetCore.Mvc;

namespace G74.Services;

public class UserAppService
{
    private readonly IRepoUser _repoUser;
    private readonly UserToDtoMapper _userToDtoMapper;
    private readonly IConfiguration _configuration;

    public UserAppService(IRepoUser repoUser, UserToDtoMapper userToDtoMapper, IConfiguration configuration)
    {
        _userToDtoMapper = userToDtoMapper;
        _repoUser = repoUser;
        _configuration = configuration;
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
    
    public async Task<UserDto> UpdateUserLimited(string email, UserDto updatedInfoUserDto)
    {
        var user = _repoUser.GetUserByEmail(email).Result;

        ArgumentNullException.ThrowIfNull(user);

        if (updatedInfoUserDto.Username != null)
        {
            user.UpdateUsername(new Username(updatedInfoUserDto.Username));
        }

        if (updatedInfoUserDto.Email != null)
        {
            user.UpdateEmail(new Email(updatedInfoUserDto.Email));
        }

        if (updatedInfoUserDto.Role != null)
        {
            user.UpdateRole(updatedInfoUserDto.Role);
        }

        var updatedUser = _repoUser.UpdateUser(user).Result;

        if (updatedUser == null)
        {
            throw new InvalidOperationException("Could not update user info");
        }

        return _userToDtoMapper.UserToDto(updatedUser);
    }

    public async Task MarkUserToBeDeleted(string userEmail)
    {
        var existingUser = _repoUser.GetUserByEmail(userEmail).Result;

        if (existingUser == null)
        {
            throw new InvalidOperationException("User not found");
        }

        string time = _configuration["GPRD:RetainInfoPeriod"] ?? "2m";

        TimeSpan retainInfoPeriod;
        if (time.EndsWith("m"))
        {
            retainInfoPeriod = TimeSpan.FromMinutes(double.Parse(time.TrimEnd('m')));
        }
        else if (time.EndsWith("h"))
        {
            retainInfoPeriod = TimeSpan.FromHours(double.Parse(time.TrimEnd('h')));
        }
        else if (time.EndsWith("d"))
        {
            retainInfoPeriod = TimeSpan.FromDays(double.Parse(time.TrimEnd('d')));
        }
        else
        {
            throw new ArgumentException("Invalid time format. Use 'm' for minutes, 'h' for hours, or 'd' for days.");
        }

        await _repoUser.MarkUserToBeDeleted(existingUser, retainInfoPeriod);
    }
    
}
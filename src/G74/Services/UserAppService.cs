using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Mappers;

namespace G74.Services;

public class UserAppService
{
    private readonly UserMapper _userMapper;
    private readonly IRepoUser _repoUser;
    private readonly UserToDTO _userToDto;

    public UserAppService(UserMapper userMapper, IRepoUser repoUser, UserToDTO userToDto)
    {
        _userToDto = userToDto;
        _userMapper = userMapper;
        _repoUser = repoUser;
    }
    
    public async Task<UserDTO> Create(UserDTO uDto)
    {
        bool bExists = await _repoUser.UserExists(uDto.Email);
        if(bExists) {
            Console.WriteLine("User already exists with the given email.");
            return null;
        }
        User user = _userMapper.Create(uDto);
        User userSaved = await _repoUser.Save(user);
        UserDTO userDto = _userToDto.DomainToDTO(userSaved);
        return userDto;
    }
    
    public async Task<UserDTO> GetUserByEmail(string email)
    {
        var existingUser = await _repoUser.GetUserByEmail(email);

        return _userToDto.DomainToDTO(existingUser);
    }
    
}
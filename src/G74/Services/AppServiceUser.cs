using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Mappers;

namespace G74.Services;

public class AppServiceUser
{
    private readonly UserMapper _userMapper;
    private readonly IRepoUser _repoUser;

    public AppServiceUser(UserMapper userMapper, IRepoUser repoUser)
    {
        _userMapper = userMapper;
        _repoUser = repoUser;
    }
    
    public async Task<UserDTO> Create(UserDTO uDto)
    {
        bool bExists = await _repoUser.UserExists(uDto.Email.ToString());
        if(bExists) {
            Console.WriteLine("User already exists with the given email.");
            return null;
        }
        User user = _userMapper.Create(uDto);
        User userSaved = await _repoUser.Save(user);
        UserDTO userDto = UserToDTO.DomainToDTO(userSaved);
        return userDto;
    }
    
    public async Task<UserDTO> GetUserByEmail(string email)
    {
        var existingUser = await _repoUser.GetUserByEmail(email);

        return UserToDTO.DomainToDTO(existingUser);
    }
    
}
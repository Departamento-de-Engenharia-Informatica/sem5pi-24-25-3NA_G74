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
    
    public User Create(VoUser voUser)
    {
        User user = null;
        user = UserMapper.Create(voUser);
        return Save(user);
    }

    public User Save(User user)
    {
        return _repoUser.Save(user);
    }
}
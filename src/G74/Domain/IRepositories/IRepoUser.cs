using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;

namespace G74.Domain.IRepositories;

public interface IRepoUser
{
    Task<User> Save(User user);

    Task<User> GetUserByEmail(string email);
    Task<bool> UserExists(string email);

}
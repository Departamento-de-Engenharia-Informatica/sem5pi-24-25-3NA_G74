using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.IRepositories;

public interface IRepoUser
{
    Task<User> Save(User user);
    Task<User> GetUserByEmail(string email);
    Task<bool> UserExists(Email email);
    Task<User> GetUserByEmail(object value);
}
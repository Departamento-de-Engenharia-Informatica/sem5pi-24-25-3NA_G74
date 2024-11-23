using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Domain.IRepositories;

public interface IRepoUser
{
    Task<User> Save(User user);
    Task<User> GetUserByEmail(string email);
    Task<bool> UserExists(string email);
    Task<User> UpdateUser(string email, User updatedUser);
    Task MarkUserToBeDeleted(User user, TimeSpan retainInfoPeriod);
    Task ExportUserDataToProlog();
}
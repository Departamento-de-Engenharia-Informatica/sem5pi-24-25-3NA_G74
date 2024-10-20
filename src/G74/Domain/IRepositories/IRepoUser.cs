using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;

namespace G74.Domain.IRepositories;

public interface IRepoUser
{
    User Save(User user);

    User GetUserByEmail(Email email);

}
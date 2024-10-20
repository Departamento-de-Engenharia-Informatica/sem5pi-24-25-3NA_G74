using G74.Domain.Aggregates.User;

namespace G74.Domain.IRepositories;

public interface IRepoUser
{
    User Save(User user);
}
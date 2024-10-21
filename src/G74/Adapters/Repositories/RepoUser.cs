using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Mappers;
using Microsoft.EntityFrameworkCore.ChangeTracking;

namespace G74.Adapters.Repositories;

public class RepoUser : GenericRepository<User>, IRepoUser
{
    private readonly UserToDataMapper _userToDataMapper;

    public RepoUser(BackofficeAppDbContext context, UserToDataMapper userToDataMapper) : base(context!)
    {
        _userToDataMapper = userToDataMapper;
    }

    public async Task<User> Save(User user)
    {
        try
        {
            DataUser dataUser = _userToDataMapper.MapToDataUser(user);
            EntityEntry<DataUser> userEntityEntry = _context.Set<DataUser>().Add(dataUser);
            await _context.SaveChangesAsync();
            DataUser savedData = userEntityEntry.Entity;
            User userSaved = _userToDataMapper.MapToUser(savedData);
            return userSaved;
        }
        catch
        {
            throw;
        }
    }
    
    public User GetUserByEmail(Email email)
    {
        throw new NotImplementedException();
        
    }
    
    
}
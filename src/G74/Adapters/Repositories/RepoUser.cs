using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;
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
    
    public async Task<User> GetUserByEmail(string email)
    {
        try {
            DataUser dataUser = await _context.Set<DataUser>()
                .FirstAsync(c => c.Email == email);

            User user = _userToDataMapper.MapToUser(dataUser);

            return user;
        }
        catch
        {
            return null;
            throw;
        }
    }
    
    public async Task<bool> UserExists(string email)
    {
        return await _context.Set<DataUser>().AnyAsync(e => e.Email == email);
    }

    public Task<User> GetUserByEmail(object value)
    {
        throw new NotImplementedException();
    }
}
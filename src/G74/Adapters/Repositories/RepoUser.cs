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
            UserDataModel userDataModel = _userToDataMapper.MapToDataUser(user);
            EntityEntry<UserDataModel> userEntityEntry = _context.Set<UserDataModel>().Add(userDataModel);
            await _context.SaveChangesAsync();
            UserDataModel savedUserData = userEntityEntry.Entity;
            User userSaved = _userToDataMapper.MapToUser(savedUserData);
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
            UserDataModel userDataModel = await _context.Set<UserDataModel>()
                .FirstAsync(c => c.Email == email);

            User user = _userToDataMapper.MapToUser(userDataModel);

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
        return await _context.Set<UserDataModel>().AnyAsync(e => e.Email == email);
    }
    
}
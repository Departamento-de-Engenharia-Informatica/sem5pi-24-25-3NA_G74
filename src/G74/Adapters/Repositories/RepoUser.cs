using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Infrastructure.Shared;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;

namespace G74.Adapters.Repositories;

public class RepoUser : GenericRepository<User>, IRepoUser
{
    private readonly UserToDataModelMapper _userToDataModelMapper;
    private readonly BackofficeAppDbContext _context;

    public RepoUser(BackofficeAppDbContext context, UserToDataModelMapper userToDataModelMapper) : base(context!)
    {
        _userToDataModelMapper = userToDataModelMapper;
        _context = context;
    }

    public async Task<User> Save(User user)
    {
        try
        {
            UserDataModel userDataModel = _userToDataModelMapper.MapToDataModel(user);
            EntityEntry<UserDataModel> userEntityEntry = _context.Set<UserDataModel>().Add(userDataModel);
            await _context.SaveChangesAsync();
            UserDataModel savedUserDataModel = userEntityEntry.Entity;
            User savedUser = _userToDataModelMapper.MapToUser(savedUserDataModel);
            return savedUser;
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

            User user = _userToDataModelMapper.MapToUser(userDataModel);

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
        return await _context.Users
            .AnyAsync(u => u.Email.Equals(email));
    }


}
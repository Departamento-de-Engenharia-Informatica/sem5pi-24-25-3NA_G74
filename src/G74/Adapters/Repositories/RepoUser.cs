using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.Patient;
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

    public async Task<User> UpdateUser(User updatedUser, string oldEmail)
    {
        try
        {
            var existingUserDataModel = await _context.Users.SingleOrDefaultAsync(u => u.Email == oldEmail);
            if (existingUserDataModel == null)
            {
                throw new KeyNotFoundException("User not found.");
            }
            _context.Entry(existingUserDataModel).Property(u => u.Username).CurrentValue = updatedUser.GetUsername();
            _context.Entry(existingUserDataModel).Property(u => u.Role).CurrentValue = updatedUser.GetRole();
            _context.Entry(existingUserDataModel).Property(u => u.Role).CurrentValue = updatedUser.GetEmail();
            
            _context.Users.Update(existingUserDataModel);
            await _context.SaveChangesAsync();
            User savedUser = _userToDataModelMapper.MapToUser(existingUserDataModel);
            return savedUser;
        }
        catch
        {
            return null;
            throw;
        }
    }

    public async Task MarkUserToBeDeleted(User user, TimeSpan retainInfoPeriod)
    {
        try
        {
            var userDataModel = await _context.Users.SingleOrDefaultAsync(u => u.Email == user.GetEmail());
            if (userDataModel == null)
            {
                throw new KeyNotFoundException("User not found.");
            }
            var deletionInfo = new DeletionInformation(true, retainInfoPeriod);
            _context.Entry(userDataModel).Property(u => u.DeletionInformation).CurrentValue = deletionInfo;
            await _context.SaveChangesAsync();
        }
        catch
        {
            throw;
        }
    }

}
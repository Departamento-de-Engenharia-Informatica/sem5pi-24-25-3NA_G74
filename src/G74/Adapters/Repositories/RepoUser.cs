using G74.Domain.Aggregates.User;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.DTO;
using G74.Infrastructure.Persistence;
using G74.Mappers;

namespace G74.Adapters.Repositories;

public class RepoUser : IRepoUser
{
    private readonly ModelToData _modelToData;
    private readonly IDBDriver _iDbDriver;

    public RepoUser(ModelToData modelToData, IDBDriver iDbDriver)
    {
        _modelToData = modelToData;
        _iDbDriver = iDbDriver;
    }

    private DataUser MapToDataUser(User user)
    {
        return _modelToData.MapToDataUser(user);
    }

    public User Save(User user)
    {
        DataUser dataUser = MapToDataUser(user);
        
        DataUser savedDataUser = _iDbDriver.Save(dataUser); 

        return MapToModelUser(savedDataUser);
    }

    private User MapToModelUser(DataUser savedUser)
    {
        return _modelToData.MapToUser(savedUser);
    }

    public User GetUserByEmail(Email email)
    {
        throw new NotImplementedException();
        
    }
    
    
}
using G74.DTO;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Persistence;

public class DBDriver : IDBDriver
{
    public DataUser Save(DataUser savedUser)
    {
        //TODO: Implementar comunicaçao com DB
        return savedUser;
    }
}
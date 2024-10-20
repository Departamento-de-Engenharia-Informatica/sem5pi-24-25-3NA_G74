using G74.DTO;

namespace G74.Infrastructure.Persistence;

public interface IDBDriver
{
    DataUser Save(DataUser savedUser);
}
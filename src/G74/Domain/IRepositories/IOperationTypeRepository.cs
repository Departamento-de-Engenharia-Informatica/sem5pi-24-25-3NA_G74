namespace G74.Domain.IRepositories;

public interface IOperationTypeRepository
{
    Task<bool> OperationTypeExists(string id);
}
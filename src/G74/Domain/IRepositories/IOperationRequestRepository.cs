using G74.Domain;
using G74.Domain.Shared;

public interface IOperationRequestRepository : IRepository<DataOperationRequest, Guid>
{
    Task<OperationRequest> Add(DataOperationRequest operation);
    Task<OperationRequest> GetOperationRequestByIdAsync(long id);
    Task<OperationRequest> Update(OperationRequest patient);
    Task<int> CountAsync();
}
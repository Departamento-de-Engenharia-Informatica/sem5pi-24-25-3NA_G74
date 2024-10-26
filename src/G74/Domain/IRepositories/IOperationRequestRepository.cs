using G74.Domain;
using G74.Domain.Shared;

public interface IOperationRequestRepository : IRepository<DataOperationRequest, Guid>
{
    Task<OperationRequest> Add(DataOperationRequest operation);
    Task<OperationRequest> GetOperationRequestByIdAsync(Guid id);
    Task<OperationRequest> Update(Guid id,OperationRequest patient);
    Task<int> CountAsync();
}
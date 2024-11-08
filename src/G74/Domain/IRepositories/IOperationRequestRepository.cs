using G74.Domain;
using G74.Domain.Shared;
using G74.DTO;

public interface IOperationRequestRepository : IRepository<OperationRequestDataModel, Guid>
{
    Task<OperationRequest> Add(OperationRequestDataModel operation);
    Task<OperationRequest> GetOperationRequestByIdAsync(Guid id);
    Task<OperationRequest> Update(Guid id,OperationRequest patient);
    Task<int> CountAsync();
    Task<OperationRequest> Delete(Guid id);

    Task<List<OperationRequest>> ReadAll();
}
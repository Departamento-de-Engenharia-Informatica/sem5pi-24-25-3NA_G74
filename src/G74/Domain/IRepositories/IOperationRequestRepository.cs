using G74.Domain;
using G74.Domain.Aggregates.OperationType;
using G74.Domain.Shared;
using G74.DTO;

public interface IOperationRequestRepository : IRepository<OperationRequestDataModel, Guid>
{
    Task<OperationRequest> Add(OperationRequestDataModel operation);
    Task<OperationRequest> GetOperationRequestByIdAsync(long id);
    Task<OperationRequest> Update(long id,OperationRequest patient);
    Task<int> CountAsync();
    Task<OperationRequest> Delete(long id);

    Task<List<OperationRequest>> ReadAll();
    Task<Boolean> GetOperationTypeByIdAsync(int id);
    Task ExportOperationRequestDataToProlog();
}
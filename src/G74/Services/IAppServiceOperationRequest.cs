public interface IAppServiceOperationRequest
{
    Task<OperationRequestDTO> RegisterOperationRequest(CreateOperationRequestDTO operationDto);
    Task<OperationRequestDTO> GetOperationRequestById(long id);
    Task<OperationRequestDTO> UpdateOperationRequest(Guid id, OperationRequestDTO updatedOperationDto);
    Task<OperationRequestDTO> DeleteOperationRequest(Guid id);
    Task<List<OperationRequestDTO>> Read();
    
}
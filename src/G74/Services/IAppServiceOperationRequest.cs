public interface IAppServiceOperationRequest
{
    Task<OperationRequestDTO> RegisterOperationRequest(CreateOperationRequestDTO operationDto);
    Task<OperationRequestDTO> GetOperationRequestById(long id);
    Task<OperationRequestDTO> UpdateOperationRequest(long id, OperationRequestDTO updatedOperationDto);
    Task<OperationRequestDTO> DeleteOperationRequest(long id);
    Task<List<OperationRequestDTO>> Read();
    
}
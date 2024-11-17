using Azure;
using Microsoft.OpenApi.Models;

namespace G74.Domain.IRepositories;

public interface IOperationTypeRepository
{
    Task<OperationType> CreateOperationType(OperationType operationType);
    Task<OperationType> UpdateOperationType(OperationType operationType);

    Task DeleteOperationType(int operationTypeId);

    Task<IEnumerable<OperationType>> SearchOperationTypeByFiltersAsync(string? OperationTypeID,
        string? Name, string? RequiredStaffBySpecialization, string? Duration);
    
}
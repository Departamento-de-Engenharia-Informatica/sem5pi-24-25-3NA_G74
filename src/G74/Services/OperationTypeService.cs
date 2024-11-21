using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Mappers;
using OperationType = G74.Domain.Aggregates.OperationType.OperationType;

namespace G74.Services;

public class OperationTypeService
{
    private readonly IOperationTypeRepository _operationTypeRepository;
    private readonly IConfiguration _configuration;

    private readonly OperationTypeDtoMapper _operationTypeDtoMapper;

    public OperationTypeService(IOperationTypeRepository operationTypeRepository, OperationTypeDtoMapper mapper,
        IConfiguration configuration)
    {
        _operationTypeRepository = operationTypeRepository;
        _operationTypeDtoMapper = mapper;
        _configuration = configuration;
    }

    public async Task<OperationTypeDTO> RegisterCreateOperationType(OperationTypeDTO operationTypeDto)
    {
        OperationType operationType = _operationTypeDtoMapper.OperationTypeDtoToDomain(operationTypeDto);
        OperationType createOperationType = await _operationTypeRepository.CreateOperationType(operationType);
        
        return _operationTypeDtoMapper.OperationTypeToDto(createOperationType);
    }


    public async Task<OperationTypeDTO> UpdateOperationType(string operationTypeId, OperationTypeDTO operationTypeDto)
    {
        var operationType = _operationTypeRepository.GetOperationTypeByID(int.Parse(operationTypeId)).Result;

        ArgumentNullException.ThrowIfNull(operationType);

        if (operationTypeDto.name != null)
        {
            operationType.UpdateName(new Name(operationTypeDto.name));
        }

        if (operationTypeDto.requiredStaffBySpecialization != null)
        {
            operationType.UpdateRequiredStaffBySpecialization(new RequiredStaffBySpecialization(_operationTypeDtoMapper.makeStringIntoDictionary(operationTypeDto.requiredStaffBySpecialization)));
        }

        if (operationTypeDto.duration != null)
        {
            operationType.UpdateDuration(int.Parse(operationTypeDto.duration));
        }

        var updatedOperationType = _operationTypeRepository.UpdateOperationType(operationType).Result;

        if (updatedOperationType == null)
        {
            throw new InvalidOperationException("Could not update operation type info");
        }

        return _operationTypeDtoMapper.OperationTypeToDto(updatedOperationType);
    }

    public async Task DeleteOperationType(int operationTypeId)
    {
        await _operationTypeRepository.DeleteOperationType(operationTypeId);
    }

    public async Task<IEnumerable<OperationTypeDTO>> SearchOperationTypeByFilters(OperationTypeDTO criteria)
    {
        try
        {
            Console.WriteLine($"Searching with criteria: OperationTypeID = {criteria.operationTypeID}, Name = {criteria.name}, Duration = {criteria.duration}");

            var operationTypesFound = await _operationTypeRepository.SearchOperationTypeByFiltersAsync(
                criteria.operationTypeID,
                criteria.name,
                criteria.requiredStaffBySpecialization,
                criteria.duration
            );

            Console.WriteLine($"Operation types found: {operationTypesFound.Count()}");

            // Logando a lista de OperationTypes antes de devolver ao controlador
            foreach (var op in operationTypesFound)
            {
                Console.WriteLine($"OperationType - ID: {op.operationTypeID}, Name: {op.name}, Duration: {op.duration}, RequiredStaff: {op.requiredStaffBySpecialization}");
            }

            return operationTypesFound.Select(_operationTypeDtoMapper.OperationTypeToDto).ToList() ??
                   new List<OperationTypeDTO>();
        }
        catch (Exception ex)
        {
            throw new Exception($"An error occurred while searching for operation types: {ex.Message}", ex);
        }
    }
}
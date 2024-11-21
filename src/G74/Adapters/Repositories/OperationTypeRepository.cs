using G74.Domain.Aggregates.OperationType;
using G74.Domain.IRepositories;
using G74.DTO;
using G74.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;

namespace G74.Adapters.Repositories;

public class OperationTypeRepository : GenericRepository<OperationType>, IOperationTypeRepository
{
    private readonly OperationTypeToDataModelMapper _operationTypeToDataModelMapper;

    public OperationTypeRepository(BackofficeAppDbContext context, OperationTypeToDataModelMapper operationTypeToDataModelMapper) : base(context!)
    {
        _operationTypeToDataModelMapper = operationTypeToDataModelMapper;
    }
    
        public async Task<OperationType> CreateOperationType(OperationType operationType)
    {
        OperationTypeDataModel operationTypeDataModel = _operationTypeToDataModelMapper.OperationTypeDomainToDataModel(operationType);
        EntityEntry<OperationTypeDataModel> operationTypeDataModelEntityEntry =
            _context.Set<OperationTypeDataModel>().Add(operationTypeDataModel);
        await _context.SaveChangesAsync();
        OperationTypeDataModel operationTypeDataModelSaved = operationTypeDataModelEntityEntry.Entity;
        return _operationTypeToDataModelMapper.OperationTypeDataModelToDomain(operationTypeDataModelSaved);
    }


    public async Task<OperationType?> UpdateOperationType(OperationType operationType)
    {
        OperationTypeDataModel operationTypeDataModel = await _context.Set<OperationTypeDataModel>()
            .FirstAsync(p => p.OperationTypeID == operationType.operationTypeID);
        _operationTypeToDataModelMapper.UpdateDataModel(operationTypeDataModel, operationType);
        _context.Entry(operationTypeDataModel).State = EntityState.Modified;
        await _context.SaveChangesAsync();
        return operationType;
    }

    public async Task DeleteOperationType(int operationTypeID)
    {
        var operationTypeDataModelToRemove = GetOperationTypeDataModelByID(operationTypeID).Result;

        if (operationTypeDataModelToRemove != null)
        {
            _context.Set<OperationTypeDataModel>().Remove(operationTypeDataModelToRemove);
            await _context.SaveChangesAsync();
        }
    }

    private async Task<OperationTypeDataModel?> GetOperationTypeDataModelByID(
        int operationTypeID)
    {
        var operationTypeDataModel =
            await _context.Set<OperationTypeDataModel>().FirstOrDefaultAsync(x =>
                x.OperationTypeID.Equals(operationTypeID));

        return operationTypeDataModel;
    }

    public async Task<IEnumerable<OperationType>> SearchOperationTypeByFiltersAsync(
        string? operationTypeId, 
        string? name, 
        string? requiredStaffBySpecialization, 
        string? duration)
    {
        // Criação de uma consulta base, sem nenhum filtro ainda
        IQueryable<OperationTypeDataModel> query = _context.Set<OperationTypeDataModel>();

        Console.WriteLine("Building query...");
        
        // Filtrando com base no operationTypeID
        if (!string.IsNullOrEmpty(operationTypeId) && int.TryParse(operationTypeId, out int parsedOperationTypeID))
        {
            query = query.Where(p => p.OperationTypeID == parsedOperationTypeID);
        }

        // Filtrando com base no name
        if (!string.IsNullOrEmpty(name))
        {
            query = query.Where(p => p.Name.Contains(name));  // Use Contains para permitir buscas parciais
        }

        // Filtrando com base no requiredStaffBySpecialization
        if (!string.IsNullOrEmpty(requiredStaffBySpecialization))
        {
            query = query.Where(p => p.RequiredStaffBySpecialization.Contains(requiredStaffBySpecialization));
        }

        // Filtrando com base no duration
        if (!string.IsNullOrEmpty(duration) && int.TryParse(duration, out int parsedDuration))
        {
            query = query.Where(p => p.EstimatedDuration == parsedDuration);
            Console.WriteLine($"Filtering by duration: {duration}");
        }

        // Executando a consulta no banco de dados e retornando os resultados
        var operationTypesFound = await query.ToListAsync();
        Console.WriteLine($"Found {operationTypesFound.Count} operation types in the database.");

        // Convertendo os dados retornados para o modelo de domínio (se necessário)
        return _operationTypeToDataModelMapper.ToDomain(operationTypesFound);
    }



    
    private IQueryable<OperationTypeDataModel> ApplyOperationTypeIdFilter(
        IQueryable<OperationTypeDataModel> query,
        string operationTypeID)
    {
        if (int.TryParse(operationTypeID, out int parsedId))
        {
            return query.Where(p => p.OperationTypeID == parsedId);
        }
        return query;
    }



    private static IQueryable<OperationTypeDataModel> ApplyNameFilter(
        IQueryable<OperationTypeDataModel> query,
        string name)
    {
        return query.Where(p => !string.IsNullOrEmpty(p.Name) && p.Name.Equals(name));
    }


    private static IQueryable<OperationTypeDataModel> ApplyRequiredStaffBySpecializationFilter(
        IQueryable<OperationTypeDataModel> query,
        string requiredStaffBySpecialization)
    {
        return query.Where(p => !string.IsNullOrEmpty(p.RequiredStaffBySpecialization) &&
                                p.RequiredStaffBySpecialization.Equals(requiredStaffBySpecialization));
    }


    private static IQueryable<OperationTypeDataModel> ApplyDurationFilter(
        IQueryable<OperationTypeDataModel> query,
        string duration)
    {
        if (int.TryParse(duration, out int parsedDuration))
        {
            return query.Where(p => p.EstimatedDuration == parsedDuration);
        }

        // Se a conversão falhar, não aplica nenhum filtro
        return query;
    }




    public async Task<OperationType?> GetOperationTypeByID(
        int operationTypeID)
    {
        var operationTypeDataModel = GetOperationTypeDataModelByID(operationTypeID).Result;
        return _operationTypeToDataModelMapper.OperationTypeDataModelToDomain(operationTypeDataModel);
    }
}
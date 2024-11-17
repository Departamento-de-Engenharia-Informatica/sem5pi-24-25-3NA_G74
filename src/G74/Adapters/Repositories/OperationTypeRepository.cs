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

    public async Task<IEnumerable<OperationType>> SearchOperationTypeByFiltersAsync(string? OperationTypeID,
        string? Name, string? RequiredStaffBySpecialization, string? Duration)
    {
        IQueryable<OperationTypeDataModel> myQueryable = _context.Set<OperationTypeDataModel>();
        
        if (OperationTypeID != null && OperationTypeID.Length > 0)
        {
            myQueryable = ApplyOperationTypeIdFilter(myQueryable, OperationTypeID);
        }

        if (Name != null && Name.Length > 0)
        {
            myQueryable = ApplyNameFilter(myQueryable, Name);
        }

        if (RequiredStaffBySpecialization != null && RequiredStaffBySpecialization.Length > 0)
        {
            myQueryable = ApplyRequiredStaffBySpecializationFilter(myQueryable, RequiredStaffBySpecialization);
        }

        if (Duration != null && Duration.Length > 0)
        {
            myQueryable = ApplyDurationFilter(myQueryable, Duration);
        }

        var operationTypeFound = await myQueryable.ToListAsync();

        return _operationTypeToDataModelMapper.ToDomain(operationTypeFound);
    }
    
    private IQueryable<OperationTypeDataModel> ApplyOperationTypeIdFilter(IQueryable<OperationTypeDataModel> query, string operationTypeID)
    {
        if (!string.IsNullOrEmpty(operationTypeID))
        {
            return query.Where(p => p.OperationTypeID.Equals(int.Parse(operationTypeID)));
        }
        return query;
    }


    private static IQueryable<OperationTypeDataModel> ApplyNameFilter(IQueryable<OperationTypeDataModel> query,
        string name)
    {
        if (!string.IsNullOrEmpty(name))
        {
            return query.Where(p => p.Name.Equals(name));
        }

        return query;
    }

    private static IQueryable<OperationTypeDataModel> ApplyRequiredStaffBySpecializationFilter(IQueryable<OperationTypeDataModel> query,
        string requiredStaffBySpecialization)
    {
        if (!string.IsNullOrEmpty(requiredStaffBySpecialization))
        {
            return query.Where(p => p.RequiredStaffBySpecialization.Equals(requiredStaffBySpecialization));
        }

        return query;
    }

    private static IQueryable<OperationTypeDataModel> ApplyDurationFilter(IQueryable<OperationTypeDataModel> query, string duration)
    {
        if (!string.IsNullOrEmpty(duration))
        {
            return query.Where(p => p.EstimatedDuration.Equals(duration));
        }

        return query;
    }
}
using G74.Domain.Aggregates.OperationType;
using G74.Domain.IRepositories;
using G74.Infrastructure;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

public class OperationTypeRepository : GenericRepository<OperationType>, IOperationTypeRepository
{
    private readonly OperationTypeToDataModelMapper _operationTypeToDataModelMapper;
    private readonly BackofficeAppDbContext _context;

    public OperationTypeRepository(BackofficeAppDbContext context, OperationTypeToDataModelMapper operationTypeToDataModelMapper) : base(context!)
    {
        _operationTypeToDataModelMapper = operationTypeToDataModelMapper;
        _context = context;
    }
    
    public async Task<bool> OperationTypeExists(string id)
    {
        return await _context.OperationTypes
            .AnyAsync(u => u.Id.Equals(id));
    }
    public async Task ExportOperationTypeDataToProlog()
    {
        //TODO:Terminar Rui Beloto.
    }
}
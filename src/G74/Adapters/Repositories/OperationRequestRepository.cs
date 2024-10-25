using G74.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories
{
    public class OperationRequestRepository : BaseRepository<DataOperationRequest, Guid>, IOperationRequestRepository
    {
        private readonly BackofficeAppDbContext _context;
        public OperationRequestRepository(BackofficeAppDbContext context) : base(context.OperationRequests)
    {
        _context = context;
    }

        public async Task<OperationRequest> Add(DataOperationRequest operation)
        {
             _context.OperationRequests.Add(operation);
            await _context.SaveChangesAsync();
            return  OperationRequestMapper.FromDataModelToDomain(operation);
        }


        public Task<OperationRequest> GetOperationRequestByIdAsync(long id)
        {
            throw new NotImplementedException();
        }

        public Task<OperationRequest> Update(OperationRequest patient)
        {
            throw new NotImplementedException();
        }
    }
}
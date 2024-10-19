using G74.Domain.Shared;

namespace G74.Infrastructure
{
    public class UnitOfWork : IUnitOfWork
    {
        private readonly BackofficeAppDbContext _context;

        public UnitOfWork(BackofficeAppDbContext context)
        {
            this._context = context;
        }

        public async Task<int> CommitAsync()
        {
            return await this._context.SaveChangesAsync();
        }
    }
}
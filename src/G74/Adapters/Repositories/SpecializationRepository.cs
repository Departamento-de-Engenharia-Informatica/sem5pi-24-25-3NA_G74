using G74.Domain.Aggregates.Specialization;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Specialization;
using G74.DTO;
using G74.Infrastructure;
using G74.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

public class SpecializationRepository : BaseRepository<SpecializationDataModel, Guid>, ISpecializationRepository
{
    private readonly BackofficeAppDbContext _dbContext;
    
    public SpecializationRepository(BackofficeAppDbContext dbContext) : base(dbContext.Specialization)
    {
        _dbContext = dbContext;
    }

    public async Task<IEnumerable<Specialization>> GetSpecializationAsync()
    {
        try
        {
            IEnumerable<SpecializationDataModel> specializationDataModel = await _dbContext.Set<SpecializationDataModel>()
                .ToListAsync();

            return SpecializationDataModel.ToDomain(specializationDataModel);
        }
        catch (Exception ex)
        {
            throw ex.InnerException!;
        }
    }

    public async Task<Specialization?> GetByCode(Code code)
    {
        try
        {
            SpecializationDataModel specializationDataModel = await _dbContext.Set<SpecializationDataModel>()
                .FirstAsync(s => s.Code == code.Value);

            return SpecializationDataModel.ToDomain(specializationDataModel);
        }
        catch (InvalidOperationException ex)
        {
            return null;
        }
    }

    public async Task<Specialization> Add(Specialization specialization)
    {
        SpecializationDataModel specializationDataModel = SpecializationDataModel.FromDomain(specialization);
        
        var ret = _dbContext.Set<SpecializationDataModel>().Add(specializationDataModel);
        await _dbContext.SaveChangesAsync();

        return SpecializationDataModel.ToDomain(ret.Entity);
    }
}
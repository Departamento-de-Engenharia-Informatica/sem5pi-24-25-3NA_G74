using G74.Domain.Aggregates.Specialization;
using G74.Domain.Value_Objects.Specialization;

namespace G74.Domain.IRepositories;

public interface ISpecializationRepository
{
    Task<IEnumerable<Specialization>> GetSpecializationAsync();

    Task<Specialization?> GetByCode(Code code);

    Task<Specialization> Add(Specialization specialization);
}
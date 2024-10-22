using G74.DataModel;
using G74.Domain.Shared;

namespace G74.Domain.IRepositories;

public interface IPatientRepository : IRepository<PatientDataModel, Guid>
{

    Task<Patient> Add(Patient patient);
    Task<Patient> GetPatientByIdAsync(long id);
    Task<Patient> GetPatientByEmail(string email);
    Task<Patient> Update(Patient patient);
    Task<int> CountAsync();


}
using G74.Domain.Aggregates.Patient;
using G74.Domain.Shared;

namespace G74.Domain.IRepositories;

public interface IPatientRepository : IRepository<Patient,PatientId>
{

    Task<Patient> Add(Patient patient);

    Task<Patient> GetPatientByIdAysnc(long id);

    Task<Patient> GetPatientByEmail(string email);

    Task<int> CountAsync();
    
}
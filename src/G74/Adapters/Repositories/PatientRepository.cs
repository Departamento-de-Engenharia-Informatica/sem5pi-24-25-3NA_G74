using G74.Domain;
using G74.Domain.Aggregates.Patient;
using G74.Domain.IRepositories;
using G74.Infrastructure.Shared;

namespace G74.Adapters.Repositories;

public class PatientRepository : BaseRepository<Patient, PatientId>, IPatientRepository
{

    public PatientRepository(BackofficeAppDbContext context):base(context.Patients)
    {
           
    }
    
    
    public Task<Patient> Add(Patient patient)
    {
        throw new NotImplementedException();
    }

    public Task<Patient> GetPatientByIdAysnc(long id)
    {
        throw new NotImplementedException();
        
    }

    public Task<Patient> GetPatientByEmail(string email)
    {
        
        throw new NotImplementedException();
    }

    
}
using G74.DataModel;
using G74.Domain;
using G74.Domain.IRepositories;
using G74.Infrastructure.Shared;

namespace G74.Adapters.Repositories;

public class PatientRepository : BaseRepository<PatientDataModel, Guid>, IPatientRepository
{

    private readonly BackofficeAppDbContext _context;

    public PatientRepository(BackofficeAppDbContext context) : base(context.Patients)
    {
        _context = context;
    }
    
    public async Task AddPatient(PatientDataModel patient)
    {
        await AddAsync(patient);
        await _context.SaveChangesAsync();
    }

    public Task<Patient> GetPatientByIdAsync(long id)
    {
        throw new NotImplementedException();

    }

    public Task<Patient> GetPatientByEmail(string email)
    {

        throw new NotImplementedException();
    }

    public async Task<Patient> Update(Patient patient)
    {
        throw new NotImplementedException();
    }

}
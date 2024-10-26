using G74.DataModel;
using G74.Domain;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

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

    public async Task<PatientDataModel?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber)
    {
        return await _context.Patients.FirstOrDefaultAsync(x => x.MedicalRecordNumber.Equals(medicalRecordNumber));
    }

    public Task<Patient> GetPatientByEmail(string email)
    {

        throw new NotImplementedException();
    }

    public async Task UpdatePatient(PatientDataModel patient)
    {
        _context.Patients.Update(patient);
        await _context.SaveChangesAsync();
    }



}
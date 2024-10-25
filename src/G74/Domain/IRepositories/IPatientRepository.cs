using G74.DataModel;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;

namespace G74.Domain.IRepositories;

public interface IPatientRepository : IRepository<PatientDataModel, Guid>
{

    Task AddPatient(PatientDataModel patient);
    Task<Patient> GetPatientByIdAsync(long id);
    Task<Patient> GetPatientByEmail(string email);
    Task<Patient> Update(Patient patient);
    Task<int> CountAsync();
    Task<Patient> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber);


}
using G74.Adapters.Repositories;
using G74.Domain.Value_Objects.Patient;
using G74.DTO;

namespace G74.Domain.IRepositories;

public interface IPatientRepository : IGenericRepository<Patient>
{
    Task<Patient> AddPatient(Patient patient);
    Task<Patient?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber);
    Task<Patient?> UpdatePatient(Patient patient);

    Task MarkPatientToBeDeleted(Patient patient, TimeSpan retainInfoPeriod);

    Task<IEnumerable<Patient>> GetPatientsReadyForDeletion();

    Task DeletePatientDefinitive(Patient patient);

    Task<IEnumerable<Patient>> SearchPatientsByFiltersAsync(string? name,
        string? gender, string? phoneNumber, string? email, DateOfBirthDTO? dateOfBirth);

    Task<int> GetMaxMedicalRecordNumberSequentialPartAsync();
}
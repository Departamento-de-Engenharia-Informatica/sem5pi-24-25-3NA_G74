using G74.DataModel;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.DTO;

namespace G74.Domain.IRepositories;

public interface IPatientRepository : IRepository<PatientDataModel, Guid>
{

    Task AddPatient(PatientDataModel patient);
    Task<PatientDataModel?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber);
    Task UpdatePatient(PatientDataModel patient);
    Task<int> CountAsync();

    Task<List<PatientDataModel>> GetPatientsReadyForDeletion();

    Task DeletePatientDefinitive(PatientDataModel patient);
    Task<IEnumerable<PatientDataModel>> SearchPatientsByFiltersAsync(PatientFilterCriteriaDTO criteria);
    
    Task<int> GetMaxMedicalRecordNumberSequentialPartAsync();
}
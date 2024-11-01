using G74.Domain.Value_Objects.Patient;
using G74.DTO;

namespace G74.Services;

public interface IPatientAppService
{
    Task<PatientDTO> RegisterPatient(PatientDTO patientDto);
    Task<PatientDTO> UpdatePatientLimited(string medicalRecordNumber, PatientDTO updatedInfoPatientDto);
    Task MarkPatientToBeDeleted(string medicalRecordNumber);
    Task<PatientDTO?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber);
    Task<IEnumerable<PatientDTO>> SearchPatientsByFilters(PatientDTO criteria);
}



using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;

namespace G74.Domain.DomainServices;

public class MedicalRecordNumberGenerator : IMedicalRecordNumberGenerator
{

    private IPatientRepository _patientRepository;

    public MedicalRecordNumberGenerator(IPatientRepository patientRepository)
    {
        _patientRepository = patientRepository;

    }
    
    
    public async Task<MedicalRecordNumber> GenerateMedicalNumber()
    {
        var totalNumberOfPatients = await _patientRepository.CountAsync();


        string year = DateTime.Now.Year.ToString();
        string month = DateTime.Now.Month.ToString();

        string sequentialNumber = (totalNumberOfPatients + 1).ToString("D5");

        string medicalRecordNumber = $"{year}{month}{sequentialNumber}";

        return new MedicalRecordNumber(medicalRecordNumber);

    }
    
}
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;

namespace G74.Domain.DomainServices;

public class MedicalRecordNumberGenerator : IMedicalRecordNumberGenerator
{
    private readonly IPatientRepository _patientRepository;

    public MedicalRecordNumberGenerator(IPatientRepository patientRepository)
    {
        _patientRepository = patientRepository ?? throw new ArgumentNullException(nameof(patientRepository));
    }

    public async Task<MedicalRecordNumber> GenerateMedicalNumber()
    {
        var totalNumberOfPatients = await _patientRepository.GetMaxMedicalRecordNumberSequentialPartAsync();

        if (totalNumberOfPatients < 0)
        {
            throw new InvalidOperationException(
                "The sequential number of the medical record number cannot be less than zero");
        }

        var year = DateTime.Now.Year.ToString();
        var month = DateTime.Now.Month.ToString("D2");

        var sequentialNumber = (totalNumberOfPatients + 1).ToString("D6");

        var medicalRecordNumber = $"{year}{month}{sequentialNumber}";

        return new MedicalRecordNumber(medicalRecordNumber);
    }
}
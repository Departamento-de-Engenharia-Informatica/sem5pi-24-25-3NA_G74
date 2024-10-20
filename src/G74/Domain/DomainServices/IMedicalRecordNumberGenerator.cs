using G74.Domain.Value_Objects.Patient;

namespace G74.Domain.DomainServices;

public interface IMedicalRecordNumberGenerator
{
    Task<MedicalRecordNumber> GenerateMedicalNumber();
}
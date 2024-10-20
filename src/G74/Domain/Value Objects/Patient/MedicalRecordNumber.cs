using System.Text.RegularExpressions;
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class MedicalRecordNumber : IValueObject
{
    public string MedicalNumber { get; }


    public MedicalRecordNumber(string medicalNumber)
    {
        
        MedicalNumber = medicalNumber;
    }

    
}
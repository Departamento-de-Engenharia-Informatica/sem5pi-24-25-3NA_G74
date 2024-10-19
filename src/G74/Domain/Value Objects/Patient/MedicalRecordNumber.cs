using System.Text.RegularExpressions;
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class MedicalRecordNumber : IValueObject
{
    public string MedicalNumber { get; }


    public MedicalRecordNumber(string medicalNumber)
    {
        if (!isValidMedicalRecordNumber(medicalNumber)) throw new ArgumentException(InvalidMedicalRecordNumberMsg);
        
        MedicalNumber = medicalNumber;
    }

    private bool isValidMedicalRecordNumber(string medicalNumber)
    {

        return Regex.IsMatch(medicalNumber, MedicalRecordNumberValidationPattern);
        
    }

    private const string MedicalRecordNumberValidationPattern = @"([0-9]{4})([0-9]{2})([0-9]{5})";

    private const string InvalidMedicalRecordNumberMsg = "Invalid Medical Record Number";
}
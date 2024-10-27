using System.Text.RegularExpressions;
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class MedicalRecordNumber : IValueObject
{
    public string MedicalNumber { get; }


    public MedicalRecordNumber(string medicalNumber)
    {
        if (!IsValidMedicalRecordNumber(medicalNumber)) throw new ArgumentException(InvalidMedicalNumberMsg);

        MedicalNumber = medicalNumber;
    }

    private bool IsValidMedicalRecordNumber(string medicalNumber)
    {
        // Matches "YYYYMMDDDDD" pattern, where MM is from 01 to 12 and DDDDD is a 5-digit number
        return Regex.IsMatch(medicalNumber, MedicalRecordNumberValidationPattern);
    }

    private const string MedicalRecordNumberValidationPattern = @"^\d{4}(0[1-9]|1[0-2])\d{5}$";

    private const string InvalidMedicalNumberMsg =
        "Invalid medical record number format. Expected format: YYYYMMDDDDD.";

    public override bool Equals(object obj)
    {
        if (obj is MedicalRecordNumber other)
        {
            return MedicalNumber == other.MedicalNumber;
        }
        return false;
    }

    public override int GetHashCode()
    {
        return MedicalNumber.GetHashCode();
    }
    
    
    public override string ToString()
    {
        return MedicalNumber;
    }
}
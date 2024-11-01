using System.Text.RegularExpressions;
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class MedicalRecordNumber : IValueObject, IEquatable<MedicalRecordNumber>
{
    public string MedicalNumber { get; }

    private const string MedicalRecordNumberValidationPattern = @"^\d{4}(0[1-9]|1[0-2])\d{6}$";

    private const string InvalidMedicalNumberMsg =
        "Invalid medical record number format. Expected format: YYYYMMnnnnnn.";

    public MedicalRecordNumber(string medicalNumber)
    {
        if (!IsValidMedicalRecordNumber(medicalNumber)) throw new BusinessRuleValidationException(InvalidMedicalNumberMsg);

        MedicalNumber = medicalNumber;
    }

    private bool IsValidMedicalRecordNumber(string medicalNumber)
    {
        // Matches "YYYYMMDDDDD" pattern, where MM is from 01 to 12 and DDDDD is a 6-digit number
        return Regex.IsMatch(medicalNumber, MedicalRecordNumberValidationPattern);
    }


    public bool Equals(MedicalRecordNumber? other) => other != null && MedicalNumber == other.MedicalNumber;

    public override bool Equals(object? obj) => obj is MedicalRecordNumber other && Equals(other);

    public override int GetHashCode() => MedicalNumber.GetHashCode();

    public override string ToString() => MedicalNumber;
}
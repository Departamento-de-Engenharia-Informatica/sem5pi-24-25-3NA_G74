using System.Text.RegularExpressions;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain.Value_Objects.Patient;

public class EmergencyContact : IValueObject, IEquatable<EmergencyContact>
{
    public string PhoneNumber { get; }

    public Name Name { get; }

    private const string PhoneNumberValidationPattern = @"^(\+351)? ?(9[1236][0-9]) ?([0-9]{3}) ?([0-9]{3})$";

    private const string InvalidPhoneNumberMsg = "Invalid portuguese phone number";

    private const string EmptyPhoneNumberMsg = "Phone number cannot be empty or whitespace";


    public EmergencyContact(string phoneNumber, Name name)
    {
        PhoneNumber = ValidatePhoneNumber(phoneNumber);

        Name = name;
    }

    public EmergencyContact(EmergencyContact other)
    {
        ValidatePhoneNumber(other.PhoneNumber);

        PhoneNumber = other.PhoneNumber;

        Name = other.Name;
    }

    public static string ValidatePhoneNumber(string phoneNumber)
    {
        if (string.IsNullOrWhiteSpace(phoneNumber)) throw new BusinessRuleValidationException(EmptyPhoneNumberMsg);

        if (!IsValidPhoneNumber(phoneNumber)) throw new BusinessRuleValidationException(InvalidPhoneNumberMsg);

        return phoneNumber;
    }

    private static bool IsValidPhoneNumber(string phoneNumber)
    {
        return Regex.IsMatch(phoneNumber, PhoneNumberValidationPattern);
    }

    public override string ToString() => $"{Name} ({PhoneNumber})";

    public bool Equals(EmergencyContact? other) =>
        other != null && PhoneNumber == other.PhoneNumber && Name.Equals(other.Name);

    public override bool Equals(object? obj) => obj is EmergencyContact other && Equals(other);

    public override int GetHashCode() => HashCode.Combine(PhoneNumber, Name);
}
using System.Text.RegularExpressions;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.Value_Objects.Patient;

public class ContactInformation : IValueObject
{
    public string PhoneNumber { get; }

    public Email EmailAddress { get; }

    private const string InvalidPhoneNumberMsg = "Invalid portuguese phone number";

    private const string PhoneNumberValidationPattern = @"^(\+351)? ?(9[1236][0-9]) ?([0-9]{3}) ?([0-9]{3})$";

    private string Email => EmailAddress.email;


    public ContactInformation(string phoneNumber, Email emailAddress)
    {
        PhoneNumber = ValidatePhoneNumber(phoneNumber);

        EmailAddress = emailAddress;
    }

    public ContactInformation(ContactInformation other)
    {
        ValidatePhoneNumber(other.PhoneNumber);

        PhoneNumber = other.PhoneNumber;

        EmailAddress = other.EmailAddress;
    }

    public static string ValidatePhoneNumber(string phoneNumber)
    {
        if (!IsValidPhoneNumber(phoneNumber)) throw new ArgumentException(InvalidPhoneNumberMsg);

        return phoneNumber;
    }

    private static bool IsValidPhoneNumber(string phoneNumber)
    {
        return Regex.IsMatch(phoneNumber, PhoneNumberValidationPattern);
    }


    public static ContactInformation FromString(string contactInfoStr)
    {
        var parts = contactInfoStr.Split(';');
        if (parts.Length != 2)
            throw new FormatException("Contact information must be in the format 'Phone;Email'.");

        var phoneNumber = parts[0];
        Email email;
        try
        {
            email = new Email((parts[1].Trim()));
        }
        catch (ArgumentException ex)
        {
            throw new BusinessRuleValidationException(ex.Message);
        }

        return new ContactInformation(phoneNumber, email);
    }

    public override string ToString()
    {
        return $"{PhoneNumber};{Email}";
    }

    public bool Equals(ContactInformation? other) =>
        other != null && PhoneNumber == other.PhoneNumber && Email.Equals(other.Email);

    public override bool Equals(object? obj) => obj is ContactInformation other && Equals(other);

    public override int GetHashCode() => HashCode.Combine(PhoneNumber, Email);
}
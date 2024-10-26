using System.Text.RegularExpressions;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.Value_Objects.Patient;

public class ContactInformation : IValueObject
{
    public string PhoneNumber { get; }

    public Email EmailAddress { get; }


    public ContactInformation(string phoneNumber, Email emailAddress)
    {

        ContactInformationValidations(phoneNumber);

        PhoneNumber = phoneNumber;

        EmailAddress = emailAddress;
    }

    public ContactInformation(ContactInformation other)
    {

        ContactInformationValidations(other.PhoneNumber);

        PhoneNumber = other.PhoneNumber;

        EmailAddress = other.EmailAddress;
    }

    private void ContactInformationValidations(string phoneNumber)
    {
        if (!IsValidPhoneNumber(phoneNumber)) throw new ArgumentException(InvalidPhoneNumberMsg);
    }

    private bool IsValidPhoneNumber(string phoneNumber)
    {
        return Regex.IsMatch(phoneNumber, PhoneNumberValidationPattern);
    }

    private const string InvalidPhoneNumberMsg = "Invalid portuguese phone number";

    private const string PhoneNumberValidationPattern = @"^(\+351)? ?(9[1236][0-9]) ?([0-9]{3}) ?([0-9]{3})$";

    public static ContactInformation FromString(string contactInfoStr)
    {
        var parts = contactInfoStr.Split(';');
        if (parts.Length != 2)
            throw new FormatException("Contact information must be in the format 'Phone;Email'.");

            
        var phone = parts[0].Trim(); 
        var email = new Email((parts[1].Trim())); 

        return new ContactInformation(phone, email);
    }

    public override string ToString()
    {
        return $"{PhoneNumber};{EmailAddress.ToString()}";
    }
}
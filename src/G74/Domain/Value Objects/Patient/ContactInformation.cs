using System.Text.RegularExpressions;
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class ContactInformation : IValueObject
{
    private string PhoneNumber { get; }

    public Email EmailAddress { get; }


    public ContactInformation(string phoneNumber, Email emailAddress)
    {
        
        ContactInformationValidations(phoneNumber);

        PhoneNumber = phoneNumber;

        EmailAddress = emailAddress;
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

    private const string PhoneNumberValidationPattern = @"^(\+351)? (9[1236][0-9]) ?([0-9]{3}) ?([0-9]{3})$";
}
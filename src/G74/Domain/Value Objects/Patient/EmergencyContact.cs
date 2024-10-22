using System.Text.RegularExpressions;
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class EmergencyContact : IValueObject
{

    private string _phoneNumber { get; }


    public EmergencyContact(string phoneNumber)
    {

        EmergencyContactValidations(phoneNumber);

        _phoneNumber = phoneNumber;
    }

    public EmergencyContact(EmergencyContact other)
    {

        EmergencyContactValidations(other._phoneNumber);

        _phoneNumber = other._phoneNumber;
    }

    private void EmergencyContactValidations(string phoneNumber)
    {
        if (!IsValidPhoneNumber(phoneNumber)) throw new ArgumentException(InvalidPhoneNumberMsg);
    }

    private bool IsValidPhoneNumber(string phoneNumber)
    {
        return Regex.IsMatch(phoneNumber, PhoneNumberValidationPattern);
    }
    
    public static EmergencyContact FromString(string phoneNumber)
    {
        if (string.IsNullOrWhiteSpace(phoneNumber))
            throw new ArgumentNullException(nameof(phoneNumber), "Contact information cannot be null or empty.");

        
        return new EmergencyContact(phoneNumber);
    }
    
    
    
    

    private const string InvalidPhoneNumberMsg = "Invalid portuguese phone number";

    private const string PhoneNumberValidationPattern = @"^(\+351)? (9[1236][0-9]) ?([0-9]{3}) ?([0-9]{3})$";



}
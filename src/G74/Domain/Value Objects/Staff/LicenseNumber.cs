using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff;

public class LicenseNumber : IValueObject
{
    public string Value { get; }

    public LicenseNumber(string value)
    {
        if (!IsValidLicense(value))
            throw new BusinessRuleValidationException("Invalid license number");
        
        Value = value;
    }

    private bool IsValidLicense(string license)
    {
        if (license == "")
        {
            return false;
        }
        
        return true;
    }
}
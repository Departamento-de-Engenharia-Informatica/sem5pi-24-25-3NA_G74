using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff;

public class PhoneNumber : IValueObject
{
    public string Value { get; }

    public PhoneNumber(string value)
    {
        if (!IsValidPhone(value))
            throw new BusinessRuleValidationException("Invalid phone number");
        
        Value = value;
    }
    
    private bool IsValidPhone(string phone)
    {
        // TODO: validation
        return true;
    }
}
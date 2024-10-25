using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff;

public class Email
{
    public string Value { get; }

    public Email(string value)
    {
        if (!IsValid(value))
            throw new BusinessRuleValidationException("Invalid email");
        
        Value = value;
    }
    
    private bool IsValid(string email)
    {
        // TODO: validation
        return true;
    }
}
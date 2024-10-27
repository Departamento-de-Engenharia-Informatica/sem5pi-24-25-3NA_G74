using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff;

public class Status
{
    public string Value { get; }

    public Status(string value)
    {
        if (!IsValid(value))
            throw new BusinessRuleValidationException("Invalid status");
        
        Value = value;
    }

    private bool IsValid(string value)
    {
        // TODO: validation
        return true;
    }
}
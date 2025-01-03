using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Specialization;

public class Designation : IValueObject
{
    public string Value { get; }

    public Designation(string value)
    {
        if (!IsValid(value))
            throw new BusinessRuleValidationException("Invalid designation");
        
        Value = value;
    }

    private bool IsValid(string value)
    {
        // TODO: validation
        return true;
    }

}
using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff;

public class StaffSpecialization
{
    public string Value { get; }

    public StaffSpecialization(string value)
    {
        if (!IsValid(value))
            throw new BusinessRuleValidationException("Invalid specialization");
        
        Value = value;
    }

    private bool IsValid(string value)
    {
        // TODO: validation
        return true;
    }
}
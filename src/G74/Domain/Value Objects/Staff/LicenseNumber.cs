using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff;

public class LicenseNumber : EntityId, IValueObject
{

    public LicenseNumber(string value) : base(value)
    {
        if (!IsValidLicense(value))
            throw new BusinessRuleValidationException("Invalid license number");
    }

    private bool IsValidLicense(string license)
    {
        if (license == "")
        {
            return false;
        }
        return true;
    }

    protected override object CreateFromString(string text)
    {
        return new LicenseNumber(text);
    }

    public override string AsString()
    {
        return Value.ToString();
    }
}
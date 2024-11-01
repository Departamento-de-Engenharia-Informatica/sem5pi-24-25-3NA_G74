using G74.Domain.Shared;


namespace G74.Domain.Value_Objects.SharedValueObjects;

public class Name : IValueObject, IEquatable<Name>
{
    public string Value { get; }

    public Name(string name)
    {
        Value = ValidateAndTrimName(name);
    }


    public static string ValidateAndTrimName(string name)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new BusinessRuleValidationException(IsNullOrWithSpaceMsg);

        return name.Trim();
    }

    public static Name FromString(string nameStr)
    {
        return new Name(nameStr);
    }


    public override string ToString() => Value;

    public bool Equals(Name? other) => other != null && Value == other.Value;

    public override bool Equals(object? obj) => obj is Name other && Equals(other);

    public override int GetHashCode() => Value.GetHashCode();


    private const string IsNullOrWithSpaceMsg = "Name cannot be empty or spaces";
}
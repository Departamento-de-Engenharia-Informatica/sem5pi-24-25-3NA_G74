using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.SharedValueObjects;

public class Gender : IValueObject, IEquatable<Gender>
{
    public string GenderDescription { get; }

    public enum GenderEnum
    {
        Male,

        Female,

        Other
    }

    public Gender(GenderEnum gender)
    {
        GenderDescription = gender.ToString();
    }

    public static Gender FromString(string genderStr)
    {
        if (!Enum.TryParse<GenderEnum>(genderStr, true, out var gender))
        {
            throw new BusinessRuleValidationException("Invalid gender");
        }

        return new Gender(gender);
    }


    public override string ToString() => GenderDescription;

    public bool Equals(Gender? other) => other != null && GenderDescription == other.GenderDescription;

    public override bool Equals(object? obj) => obj is Gender other && Equals(other);

    public override int GetHashCode() => GenderDescription.GetHashCode();
}
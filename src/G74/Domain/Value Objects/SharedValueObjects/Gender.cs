using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.SharedValueObjects;

public class Gender : IValueObject
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
}